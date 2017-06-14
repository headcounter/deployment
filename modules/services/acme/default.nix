{ config, pkgs, lib, hclib, ... }:

let
  inherit (lib) mkOption types;
  cfg = config.headcounter.services.acme;
  acmetool = pkgs.headcounter.acmetool.bin;
  certDir = "/run/ssl-certs";

  domainModule.options = {
    users = mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        Users that have access to the private key and certificate.
      '';
    };

    otherDomains = mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        Alternative fully qualified domain names which to issue the
        certificate for.
      '';
    };

    reloads = mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        The service units to reload, without the <literal>.service</literal>
        suffix.
      '';
    };

    restarts = mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        The service units to restart, without the <literal>.service</literal>
        suffix.
      '';
    };
  };

  validator = pkgs.headcounter.compileHaskell {
    name = "validator";
    source = ./validator.hs;
    ghcflags = [ "-O2" "-Wall" ];
    buildDepends = [ "cereal" "cereal-text" pkgs.headcounter.nexus ];
  };

  hooks = pkgs.writeTextFile {
    name = "acme-hooks";
    executable = true;
    destination = "/main.hook";
    text = let
      dev = lib.optionalString (cfg.handlerDevice != null) cfg.handlerDevice;
      cat = lib.escapeShellArg "${pkgs.coreutils}/bin/cat";
    in ''
      #!${pkgs.stdenv.shell} -e
      if [ "$1" = live-updated ]; then
        umask 0077
        while read name; do
          certdir=${lib.escapeShellArg "${cfg.stateDir}/live/"}"$name"
          ${cat} "$certdir/privkey" "$certdir/fullchain" > "$certdir/full"
        done
        exit 0
      fi

      exec ${lib.escapeShellArg validator} --client \
        ${lib.escapeShellArg cfg.handlerAddress} ${toString cfg.handlerPort} \
        ${lib.escapeShellArg dev} "$@"
    '';
  };

  tosUrl = "https://letsencrypt.org/documents/LE-SA-v1.1.1-August-1-2016.pdf";
  directoryUrl = "https://acme-v01.api.letsencrypt.org/directory";

  responses = builtins.toJSON {
    acme-enter-email = if cfg.email == null then "" else cfg.email;
    "acme-agreement:${tosUrl}" = true;
    acmetool-quickstart-choose-server = directoryUrl;
    acmetool-quickstart-choose-method = "hook";
    acmetool-quickstart-key-type = cfg.key.type;
    acmetool-quickstart-rsa-key-size = cfg.key.size;
    acmetool-quickstart-ecdsa-curve = cfg.key.curve;
  };

  mkPathUnitName = domain: "acme-${builtins.hashString "sha256" domain}";
  mkPathUnit = domain: lib.const "${mkPathUnitName domain}.path";
  pathUnits = lib.mapAttrsToList mkPathUnit cfg.domains;

in {
  options.headcounter.services.acme = {
    enable = lib.mkEnableOption "ACME client";

    dnsHandler = {
      enable = lib.mkEnableOption "ACME DNS challenge handler";

      fqdn = mkOption {
        type = types.str;
        description = ''
          The FQDN this nameserver is available to serve TXT records.

          A valid NS record is needed for this, which is usually the one that's
          used in the zone that's delegating the
          <literal>_acme-challange</literal> zone to the validator.
        '';
      };

      listen = let
        desc = "incoming ACME client connections";
      in hclib.mkListenerOption desc "localhost" 9178;
    };

    handlerAddress = mkOption {
      type = types.str;
      default = "localhost";
      description = ''
        The address or hostname to connect to the DNS challenge handler.
      '';
    };

    handlerPort = mkOption {
      type = hclib.types.port;
      default = 9178;
      description = ''
        TCP Port where the DNS challenge handler is reachable.
      '';
    };

    handlerDevice = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Network device to use for connecting to the DNS challenge handler.
      '';
    };

    stateDir = mkOption {
      type = types.path;
      default = "/var/lib/acme";
      description = ''
        The directory where <command>acmetool</command> will put its state
        files.
      '';
    };

    email = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Contact email address for the CA to be able to reach you.
      '';
    };

    key = {
      type = mkOption {
        type = types.enum [ "rsa" "ecdsa" ];
        default = "rsa";
        description = ''
          The key type to use for the certificates, either
          <literal>rsa</literal> or <literal>ecdsa</literal>.
        '';
      };

      size = mkOption {
        type = types.addCheck types.int (lib.flip lib.elem [ 1024 2048 4096 ]);
        default = 2048;
        description = ''
          The RSA key size in bits. Only relevant if <option>type</option> is
          set to <literal>rsa</literal>.
        '';
      };

      curve = mkOption {
        type = types.enum [ "nistp256" "nistp384" "nistp512" ];
        default = "nistp256";
        description = ''
          The ECDSA curve to use. Only relevant if <option>type</option> is set
          to <literal>ecdsa</literal>.
        '';
      };
    };

    domains = mkOption {
      type = types.attrsOf (types.submodule domainModule);
      default = {};
      description = ''
        Domains to issue certificates for. The attribute name is the FQDN and
        the attribute values are options for the specified FQDN.
      '';
    };
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      users.users.acme = {
        description = "ACME User";
        uid = 2023;
        group = "acme";
        home = cfg.stateDir;
      };

      users.groups.acme.gid = 2023;

      _module.args.ssl = lib.mapAttrs (domain: dcfg: {
        cert = "${certDir}/${domain}/cert";
        chain = "${certDir}/${domain}/chain";
        full = "${certDir}/${domain}/full";
        fullchain = "${certDir}/${domain}/fullchain";
        privkey = "${certDir}/${domain}/privkey";
      }) cfg.domains;

      systemd.mounts = lib.singleton {
        description = "SSL Certificate Directory";
        requires = lib.singleton "acme.service" ++ pathUnits;
        after = [ "acme.service" "acme-setperms.service" ];
        before = pathUnits;
        what = "${cfg.stateDir}/live";
        where = certDir;
        type = "none";
        options = "bind,ro";
      };

      systemd.automounts = lib.singleton {
        wantedBy = [ "local-fs.target" ];
        where = certDir;
      };

      headcounter.conditions.acme.connectable = {
        address = cfg.handlerAddress;
        port = cfg.handlerPort;
      };

      systemd.services = {
        acme-init = {
          description = "Initialize ACME State Directory";
          requiredBy = [ "acme.service" ];
          before = [ "acme.service" ];
          unitConfig.ConditionPathExists = "!${cfg.stateDir}";
          serviceConfig.Type = "oneshot";
          script = ''
            mkdir -m 0711 -p ${lib.escapeShellArg cfg.stateDir}
            chown acme:acme ${lib.escapeShellArg cfg.stateDir}
          '';
        };

        acme = {
          description = "Update ACME Certificates";

          after = [ "network.target" "network-online.target" ];
          wants = [ "network-online.target" ];

          path = lib.mkForce [];

          serviceConfig = {
            Type = "oneshot";
            PrivateTmp = true;
            ExecStart = "${acmetool}/bin/acmetool reconcile --batch";
            User = "acme";
            Group = "acme";
            WorkingDirectory = cfg.stateDir;
            # While we have patched out everything that might want to listen,
            # let's make sure this is really the case.
            SystemCallFilter = "~listen";
          } // lib.optionalAttrs (cfg.handlerDevice != null) {
            AmbientCapabilities = [ "CAP_NET_RAW" ];
          };

          preStart = ''
            ${pkgs.coreutils}/bin/mkdir -p conf
            ${pkgs.coreutils}/bin/cat \
              "${pkgs.writeText "responses.json" responses}" \
              > conf/responses
            ${pkgs.coreutils}/bin/cat > conf/perm <<EOF
            . 0644 0751
            keys 0640 0711
            certs/*/full 0640 0711
            EOF
          '';

          environment.ACME_STATE_DIR = cfg.stateDir;
          environment.ACME_HOOKS_DIR = hooks;

          environment.ACME_DESIRED_DIR = let
            mkDesired = name: attrs: let
              contents = lib.escapeShellArg (builtins.toJSON attrs);
            in "echo -n ${contents} > \"$out\"/${lib.escapeShellArg name}";
            domains = lib.mapAttrsToList (name: dcfg: mkDesired name {
              satisfy.names = lib.singleton name ++ dcfg.otherDomains;
            }) cfg.domains;
          in pkgs.runCommand "acme-desired" {} ''
            mkdir "$out"
            ${lib.concatStringsSep "\n" domains}
          '';
        } // lib.optionalAttrs (cfg.handlerDevice != null) {
          bindsTo = [
            "sys-subsystem-net-devices-${cfg.handlerDevice}.device"
          ];
        };

        acme-setperms = {
          description = "Set Permissions On ACME Certificates";
          requiredBy = [ "acme.service" ];
          after = [ "acme.service" ];
          serviceConfig.Type = "oneshot";
          script = let
            inherit (lib) escapeShellArg;
            unlines = lib.concatStringsSep "\n";
            setPerms = domain: { users, ... }: let
              perms = lib.concatMapStringsSep "," (u: "u:${u}:r") users;
              mkFile = x: escapeShellArg "${cfg.stateDir}/live/${domain}/${x}";
              files = [ "cert" "chain" "full" "fullchain" "privkey" ];
              setfacl = lib.escapeShellArg "${pkgs.acl.bin}/bin/setfacl";
              setPerm = f: "${setfacl} -L -b -m ${escapeShellArg perms} ${f}";
            in unlines (map (x: setPerm (mkFile x)) files);
          in unlines (lib.mapAttrsToList setPerms cfg.domains);
        };

      } // lib.mapAttrs' (domain: dcfg: {
        name = mkPathUnitName domain;
        value = let
          mkUnit = name: lib.escapeShellArg "${name}.service";
          mkUnitList = lib.concatMapStringsSep " " mkUnit;
          maybeDo = action: units: lib.optionalString (units != []) ''
            systemctl ${action} ${mkUnitList units}
          '';
        in {
          description = "Reload/Restart Services For ACME Domain ${domain}";
          after = [ "acme-setperms.service" ];
          script = maybeDo "restart" dcfg.restarts
                 + maybeDo "reload"  dcfg.reloads;
        };
      }) cfg.domains;

      systemd.paths = lib.mapAttrs' (domain: lib.const {
        name = mkPathUnitName domain;
        value = {
          description = "Notifications For ACME Domain ${domain}";
          pathConfig.PathChanged = "${certDir}/${domain}";
        };
      }) cfg.domains;

      systemd.timers.acme = {
        description = "Hourly Update Check For ACME Certificates";
        wantedBy = [ "multi-user.target" ];
        timerConfig.OnCalendar = "hourly";
      };
    })
    (lib.mkIf cfg.dnsHandler.enable {
      headcounter.nsd-zone-writer.enable = true;
      headcounter.nsd-zone-writer.allowedUsers = [ "acme-validator" ];

      users.users.acme-validator = {
        description = "ACME Validation User";
        uid = 2025;
        group = "acme-validator";
      };

      users.groups.acme-validator.gid = 2025;

      systemd.sockets = hclib.mkSocketConfig {
        namePrefix = "acme-dns-handler";
        description = "Socket for ACME DNS Challenge Handler";
        config = cfg.dnsHandler.listen;
      };

      systemd.services.acme-dns-handler = {
        description = "ACME DNS Challenge Handler";
        after = [ "network.target" "network-online.target" ];
        serviceConfig.ExecStart = let
          inherit (config.headcounter.nsd-zone-writer) command;
          args = lib.concatMapStringsSep " " lib.escapeShellArg [
            command cfg.dnsHandler.fqdn
          ];
        in "${validator} --server ${args}";
        serviceConfig.User = "acme-validator";
        serviceConfig.Group = "acme-validator";
      };
    })
  ];
}
