{ pkgs, lib, config, ... }:

with lib;

let
  cfg = config.headcounter.services.webspace;

  openssh = lib.overrideDerivation pkgs.openssh (o: {
    patches = (o.patches or []) ++ singleton ./openssh.patch;
  });

  basedir = "/var/www";
  group = config.headcounter.services.lighttpd.group;

  sftpdConfigFile = pkgs.writeText "sftpd.config" ''
    Protocol 2
    Port 2222
    PermitRootLogin no
    ChrootDirectory ${basedir}
    Subsystem sftp ${openssh}/libexec/sftp-server
    AllowAgentForwarding no
    PermitTTY no
    PermitTunnel no
    AllowTcpForwarding no
    X11Forwarding no
    UsePAM no
    UsePrivilegeSeparation sandbox

    ${flip concatMapStrings config.services.openssh.hostKeys (k: ''
      HostKey ${k.path}
    '')}

    AllowUsers ${concatStringsSep " " (
      map (n: "webspace_${n}") (attrNames cfg.accounts)
    )}

    ${concatStrings (mapAttrsToList (name: opts: ''
    Match User webspace_${name}
      ForceCommand internal-sftp -d ${name} -u 027
    '') cfg.accounts)}
  '';
in {
  options.headcounter.services.webspace = {
    enable = mkEnableOption "headcounter webspace";

    accounts = mkOption {
      default = {};
      example = {
        funnyuser = {
          uid = 1000;
          userOptions = {
            password = "foobar";
          };
          virtualHosts = [
            { type = "regex";
              on = "^(?:[a-z]+\\.)?example\\.com\$";
            }
            { type = "static";
              on = "example.org";
            }
          ];
        };
      };
      type = types.attrsOf (types.submodule {
        options.uid = mkOption {
          type = types.uniq (mkOptionType {
            name = "int between 1000 and 5000";
            check = uid: isInt uid && uid >= 1000 && uid <= 5000;
            merge = mergeOneOption;
          });
          example = 1000;
          description = ''
            User ID for the webspace user, has to be between 1000 and 5000.
          '';
        };

        options.userOptions = mkOption {
          default = {};
          example = {
            password = "abcde";
            extraGroups = "mail";
          };
          type = types.attrs;
          description = ''
            Extra user options that will be merged with
            <option>users.extraUsers</option>.

            Note that the attributes <option>uid</option>,
            <option>home</option>, <option>group</option> and
            <option>createHome</option> can't be set here.
          '';
        };

        options.virtualHosts = mkOption {
          default = [];
          example = singleton {
            type = "static";
            on = "www.example.com";
          };
          type = types.listOf types.attrs;
          description = ''
            Virtual hosts to be routed to this webspace account.
            The options are the same as
            <option>headcounter.services.lighttpd.virtualHosts</option>.
          '';
        };
      });
      description = ''
        Webspace account and virtual host definitions. All
        <option>userOptions</option> are merged with values of
        <option>users.extraUsers</option> and <option>virtualHosts</option> are
        merged with <option>headcounter.services.lighttpd.virtualHosts</option>.
      '';
    };
  };

  config = mkIf cfg.enable {
    headcounter.services.lighttpd = {
      enable = true;

      configuration = ''
        mimetype.assign = (
          ".gif"  => "image/gif",
          ".jpg"  => "image/jpeg",
          ".jpeg" => "image/jpeg",
          ".png"  => "image/png",
          ".css"  => "text/css",
          ".html" => "text/html",
          ".htm"  => "text/html",
          ".js"   => "text/javascript",
          ""      => "application/octet-stream"
        )
      '';

      virtualHosts = flatten (mapAttrsToList (name: opts: map (vhost: {
        socket = ":80";
        docroot = "${basedir}/${name}";
      } // vhost) opts.virtualHosts) cfg.accounts);
    };

    systemd.services.sftpd = {
      description = "SFTP Service";
      after = [ "sshd.service" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        ExecStart = "${openssh}/bin/sshd -f \"${sftpdConfigFile}\"";
        KillMode = "process";
        Restart = "always";
        Type = "forking";
      };
    };

    users.extraUsers = mapAttrs' (name: opts: {
      name = "webspace_${name}";
      value = opts.userOptions // {
        inherit (opts) uid;
        home = "${basedir}/${name}";
        inherit group;
        createHome = false;
      };
    }) cfg.accounts;

    system.activationScripts.createWebroots = let
      scripts = mapAttrsToList (name: opts: ''
        umask 066 && mkdir -p "${basedir}/${name}"
        chmod 2710 "${basedir}/${name}"
        chown "webspace_${name}:${group}" "${basedir}/${name}"
      '') cfg.accounts;
    in stringAfter [ "etc" "users" "groups" ] (concatStrings scripts);
  };
}
