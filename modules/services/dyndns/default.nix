{ config, pkgs, lib, ... }:

let
  inherit (lib) mkOption mkEnableOption mkIf mkMerge mkDefault types;
  cfg = config.headcounter.services.dyndns;

  package = pkgs.stdenv.mkDerivation {
    name = "dyndns";
    src = ./dyndns.hs;

    ghc = pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [
      wai warp iproute stm yaml network-simple acid-state
    ]);

    buildCommand = ''
      mkdir -p "$out/bin"
      "$ghc/bin/ghc" --make "$src" -o "$out/bin/dyndns"
    '';
  };

  mkListenerOptions = desc: defHost: defPort: {
    host = mkOption {
      type = types.str;
      default = defHost;
      description = ''
        Host/IP to listen for ${desc}.
      '';
    };

    port = mkOption {
      type = types.int;
      default = defPort;
      description = ''
        Port to listen for ${desc}.
      '';
    };
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

  userGroupOptions = mode: defaultUser: defaultGroup: {
    user = mkOption {
      type = types.nullOr types.str;
      default = defaultUser;
      description = ''
        User name to use for running the ${mode} daemon.
      '';
    };

    group = mkOption {
      type = types.nullOr types.str;
      default = defaultGroup;
      description = ''
        Group name to use for running the ${mode} daemon.
      '';
    };
  };

  mkService = mode: cfgContents: let
    cfgFile = pkgs.writeText "dyndns-${mode}.yaml" cfgContents;
    modeUCFirst = lib.toUpper (lib.substring 0 1 mode)
                + lib.substring 1 (-1) mode;
    defaultUG = if mode == "master" then "dyndns" else "root";
    userGroupSC = lib.optionalAttrs (cfg.${mode}.user != defaultUG) {
      User = cfg.slave.user;
    } // lib.optionalAttrs (cfg.${mode}.group != defaultUG) {
      Group = cfg.slave.group;
    };
    commonSC = {
      ExecStart = "@${package}/bin/dyndns dyndns-${mode} --${mode} ${cfgFile}";
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
    after = lib.singleton "networking.target";
    description = "Dynamic DNS ${modeUCFirst} Server";
    restartTriggers = [ cfgFile ];
    serviceConfig = commonSC // specificSC // userGroupSC;
  };

  yamlStr = val: "'${lib.replaceStrings ["'"] ["''"] val}'";

  credentials = let
    mkCredUser = username: attrs: [
      "  ${yamlStr username}:"
      "    password: ${yamlStr attrs.password}"
      "    domains:"
    ] ++ map (d: "      - ${yamlStr d}") attrs.domains;
    users = lib.flatten (lib.mapAttrsToList mkCredUser cfg.master.credentials);
    result = lib.concatStringsSep "\n" (lib.singleton "credentials:" ++ users);
  in lib.optionalString (users != []) result;

  nameservers = let
    mkNS = ns: "  - ${yamlStr ns}";
    nsHosts = map mkNS cfg.master.nameservers;
  in lib.concatStringsSep "\n" (lib.singleton "nameservers:" ++ nsHosts);

  masterConfig = mkIf cfg.master.enable {
    systemd.services.dyndns-master = mkService "master" ''
      ${credentials}
      ${nameservers}
      httpConfig:
        host: ${yamlStr cfg.master.http.host}
        port: ${toString cfg.master.http.port}
      slaveConfig:
        host: ${yamlStr cfg.master.slave.host}
        port: ${toString cfg.master.slave.port}
      email: ${yamlStr cfg.master.emailAddress}
      stateDir: ${yamlStr cfg.master.stateDir}
    '';

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
      opt = "headcounter.services.dyndns.master.nameservers";
    in lib.singleton {
      assertion = lib.length cfg.master.nameservers >= 1;
      message = "You have to specify at least one nameserver in ${opt}.";
    };
  };

  slaveBaseConfig = mkIf cfg.slave.enable {
    systemd.services.dyndns-slave = mkService "slave" ''
      masterHost: ${yamlStr cfg.slave.master.host}
      masterPort: ${toString cfg.slave.master.port}
      writeZoneCommand: ${yamlStr cfg.slave.zoneCommand}
    '';
  };

  slaveNSDConfig = mkIf cfg.slave.useNSD (import ./nsd.nix {
    inherit config pkgs lib;
  });

  slaveConfig = mkMerge [ slaveBaseConfig slaveNSDConfig ];

in {

  options.headcounter.services.dyndns.master = {
    enable = mkEnableOption "Headcounter dynamic DNS master service";
    http = mkListenerOptions "incoming HTTP connections" "*" 3000;
    slave = mkListenerOptions "incoming slave connections" "127.0.0.1" 6000;

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
  } // userGroupOptions "master" "dyndns" "dyndns";

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
  } // userGroupOptions "slave" "root" "root";

  config = mkMerge [ slaveConfig masterConfig ];
}
