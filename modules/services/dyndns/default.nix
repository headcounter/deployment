{ config, pkgs, lib, ... }:

let
  inherit (lib) mkOption mkEnableOption mkIf mkMerge mkDefault types;
  cfg = config.headcounter.services.dyndns;

  daemon = pkgs.headcounter.compileHaskell {
    name = "dyndns";
    source = ./dyndns.hs;

    ghcflags = [ "-O2" "-Wall" "-fno-warn-orphans" ];
    buildDepends = [
      "acid-state" "cereal-text" "iproute" "stm" "wai" "warp"
      "yaml" pkgs.headcounter.nexus
    ];
  };

  mkListenerOption = desc: defHost: defPort: mkOption {
    type = types.listOf (types.submodule {
      options.host = mkOption {
        type = types.str;
        example = "::";
        description = ''
          Hostname, IPv4 or IPv6 address to listen for ${desc}.
        '';
      };

      options.port = mkOption {
        type = types.int;
        default = defPort;
        description = "Port to listen for ${desc}.";
      };

      options.device = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "tun1000";
        description = "The network device to bind the slave sockets to.";
      };
    });

    default = lib.singleton { host = defHost; };
    example = [
      { host = "localhost"; }
      { host = "1.2.3.4"; port = 666; device = "eth0"; }
    ];

    description = "Hosts/Ports/Devices to listen for ${desc}.";
  };

  credentialOptions = { name, ... }: {
    options.username = mkOption {
      type = types.str;
      description = ''
        The user name that clients use to update their dynamic DNS records.
      '';
    };

    options.password = mkOption {
      type = types.str;
      description = ''
        The password that clients use to update their dynamic DNS records.
      '';
    };

    options.domains = mkOption {
      type = types.listOf types.str;
      description = ''
        The fully qualified domain names that this user is allowed to update.
      '';
    };

    config.username = mkDefault name;
  };

  userGroupOptions = mode: {
    user = mkOption {
      type = types.str;
      default = "dyndns";
      description = ''
        User name to use for running the ${mode} daemon.
      '';
    };

    group = mkOption {
      type = types.nullOr types.str;
      default = "dyndns";
      description = ''
        Group name to use for running the ${mode} daemon.
      '';
    };
  };

  mkService = mode: cfgContents: let
    cfgFile = pkgs.writeText "dyndns-${mode}.yaml" cfgContents;
    modeUCFirst = lib.toUpper (lib.substring 0 1 mode)
                + lib.substring 1 (-1) mode;
    userGroupSC = lib.optionalAttrs (cfg.${mode}.user != "dyndns") {
      User = cfg.slave.user;
    } // lib.optionalAttrs (cfg.${mode}.group != "dyndns") {
      Group = cfg.slave.group;
    };
    commonSC = {
      ExecStart = "@${daemon} dyndns-${mode} --${mode} ${cfgFile}";
      RestartSec = 10;
    };
    specificSC = if mode == "master" then {
      Restart = "on-failure";
      User = "dyndns";
      Group = "dyndns";
    } else {
      Restart = "always";
    };
  in {
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    description = "Dynamic DNS ${modeUCFirst} Server";
    restartTriggers = [ cfgFile ];
    serviceConfig = commonSC // specificSC // userGroupSC;
  };

  yamlStr = val: "'${lib.replaceStrings ["'"] ["''"] val}'";
  yamlList = items: "[${lib.concatMapStringsSep ", " yamlStr items}]";

  credentials = let
    mkCredUser = username: attrs: [
      "  ${yamlStr username}:"
      "    password: ${yamlStr attrs.password}"
      "    domains: ${yamlList attrs.domains}"
    ];
    users = lib.flatten (lib.mapAttrsToList mkCredUser cfg.master.credentials);
    result = lib.concatStringsSep "\n" (lib.singleton "credentials:" ++ users);
  in lib.optionalString (users != []) result;

  nameservers = let
    mkNS = ns: "  - ${yamlStr ns}";
    nsHosts = map mkNS cfg.master.nameservers;
  in lib.concatStringsSep "\n" (lib.singleton "nameservers:" ++ nsHosts);

  masterConfig = mkIf cfg.master.enable {
    systemd.services.dyndns-master = mkService "master" ''
      ${if cfg.master.credentialFile != null then ''
      credentials: !include ${yamlStr cfg.master.credentialFile}
      '' else credentials}
      ${nameservers}
      email: ${yamlStr cfg.master.emailAddress}
      stateDir: ${yamlStr cfg.master.stateDir}
    '';

    systemd.sockets = let
      mkSocketsFor = name: desc: builtins.listToAttrs (lib.imap (n: lcfg: {
        name = "dyndns-master-${name}-${toString n}";
        value = let
          isV6 = builtins.match ".*:.*" lcfg.host != null;
          host = if isV6 then "[${lcfg.host}]" else lcfg.host;
          hostPort = "${host}:${toString lcfg.port}";
        in {
          description = "${desc} (${hostPort})";
          requiredBy = [ "dyndns-master.service" ];
          socketConfig = {
            FreeBind = true;
            Service = "dyndns-master.service";
            FileDescriptorName = name;
            ListenStream = hostPort;
          } // lib.optionalAttrs (lcfg.device != null) {
            BindToDevice = lcfg.device;
          };
        };
      }) cfg.master.${name});
    in mkSocketsFor "slave" "Dyndns Slave Socket For Master"
    // mkSocketsFor "http" "Dyndns HTTP Socket For Master";

    users.users.dyndns = mkIf (cfg.master.user == "dyndns") {
      uid = 2022;
      group = cfg.master.group;
      description = "Dynamic DNS Master User";
      home = cfg.master.stateDir;
      createHome = true;
    };

    users.groups.dyndns = mkIf (cfg.master.group == "dyndns") {
      gid = 2022;
    };

    assertions = let
      mkOpt = opt: "`headcounter.services.dyndns.master.${opt}'";
    in [
      { assertion = lib.length cfg.master.nameservers >= 1;
        message = "You have to specify at least one nameserver in"
                + " ${mkOpt "nameservers"}.";
      }
      { assertion = cfg.master.credentialFile != null
                 -> cfg.master.credentials == {};
        message = "You can either specify ${mkOpt "credentials"} or"
                + " ${mkOpt "credentialFile"}, not both.";
      }
    ];
  };

  slaveBaseConfig = mkIf cfg.slave.enable {
    systemd.services.dyndns-slave = mkService "slave" ''
      masterHost: ${yamlStr cfg.slave.master.host}
      masterPort: ${toString cfg.slave.master.port}
      ${lib.optionalString (cfg.slave.master.device != null) ''
      masterDevice: ${yamlStr cfg.slave.master.device}
      ''}
      writeZoneCommand: ${yamlStr cfg.slave.zoneCommand}
    '';

    headcounter.conditions.dyndns-slave.connectable = {
      address = cfg.slave.master.host;
      inherit (cfg.slave.master) port;
    };

    users.users.dyndns = mkIf (cfg.slave.user == "dyndns") {
      description = "Dynamic DNS Slave User";
      uid = 2022;
      group = cfg.slave.group;
    };

    users.groups.dyndns = mkIf (cfg.slave.group == "dyndns") {
      gid = 2022;
    };
  };

  slaveNSDConfig = mkIf cfg.slave.useNSD {
    systemd.services.dyndns-slave.after = [ "nsd.service" ];
    headcounter.nsd-zone-writer.enable = lib.mkOverride 900 true;
    headcounter.nsd-zone-writer.beforeUnits = [ "dyndns-slave.service" ];
    headcounter.nsd-zone-writer.allowedUsers = lib.singleton cfg.slave.user;

    headcounter.services.dyndns.slave = {
      zoneCommand = let
        zoneWriter = config.headcounter.nsd-zone-writer.command;
      in lib.mkOverride 900 zoneWriter;
    };
  };

  slaveConfig = mkMerge [ slaveBaseConfig slaveNSDConfig ];

in {

  options.headcounter.services.dyndns.master = {
    enable = mkEnableOption "Headcounter dynamic DNS master service";
    http = mkListenerOption "incoming HTTP connections" "::" 3000;
    slave = mkListenerOption "incoming slave connections" "127.0.0.1" 6000;

    credentials = mkOption {
      type = types.attrsOf (types.submodule credentialOptions);
      default = {};
      example.alice.password = "ilikebob";
      example.alice.domains = [ "alice.example.net" ];
      example.bob.password = "ilikealice";
      example.bob.domains = [ "bob.example.com" "bob.example.org" ];
      description = ''
        Credentials consisting of usernames, passwords and allowed hosts that
        the master server allows.
      '';
    };

    credentialFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = ''
        Use this file for credentials instead of putting them directly into the
        main configuration file.
      '';
    };

    stateDir = mkOption {
      type = types.path;
      default = "/var/lib/dyndns";
      description = ''
        Directory where the master process keeps its internal state (which for
        example consists of zonefile serial numbers).
      '';
    };

    emailAddress = mkOption {
      type = types.str;
      description = ''
        The email address to use in the SOA record.
      '';
    };

    nameservers = mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        The DNS servers that are authoriative for all of the zones that are
        generated by the slaves.
      '';
    };
  } // userGroupOptions "master";

  options.headcounter.services.dyndns.slave = {
    enable = mkEnableOption "Headcounter dynamic DNS slave service";

    useNSD = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to update zones of a locally running NSD (see
        <option>services.nsd</option>).
      '';
    };

    zoneCommand = mkOption {
      type = types.str;
      description = ''
        Command to update the zone file.

        The FQDN of the zone is passed as the first argument and the zone file
        contents are piped to that command via <literal>stdin</literal>.
      '';
    };

    master.host = mkOption {
      type = types.str;
      default = "localhost";
      description = ''
        Master server host/IP where to receive zone updates from.
      '';
    };

    master.port = mkOption {
      type = types.int;
      default = 6000;
      description = ''
        Master server port where to receive zone updates from.
      '';
    };

    master.device = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Device to use for connecting to the master.
      '';
    };
  } // userGroupOptions "slave";

  config = mkMerge [ slaveConfig masterConfig ];
}
