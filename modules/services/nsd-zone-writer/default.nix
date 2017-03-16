{ config, pkgs, lib, ... }:

let
  setOpt = lib.mkOverride 900;

  nsdcfg = config.services.nsd.remoteControl;

  nsdStateDir = config.users.users.nsd.home;
  baseDir = "${nsdStateDir}/dynzones";

  nsdControlConfig = pkgs.writeText "nsd-control.config" ''
    remote-control:
      control-enable:    yes
      control-key-file:  "${nsdcfg.controlKeyFile}"
      control-cert-file: "${nsdcfg.controlCertFile}"
      control-port:      ${toString nsdcfg.port}
      server-cert-file:  "${nsdcfg.serverCertFile}"
  '';

  zoneWriter = pkgs.headcounter.compileC {
    name = "nsd-zone-writer";
    source = ''
      #include <pwd.h>
      #include <stdio.h>
      #include <stdlib.h>
      #include <sys/types.h>
      #include <unistd.h>

      int main(int argc, char *argv[]) {
        uid_t curuid;
        struct passwd *pw;
        char *emptyenv[] = { NULL };
        char *execargv[] = {
          argv[0], "-e", "-p", "--", "${pkgs.substituteAll {
            name = "nsd-write-zone";
            src = ./writer.sh.in;
            inotifywait = "${pkgs.inotify-tools}/bin/inotifywait";
            ctrl = "${pkgs.nsd}/bin/nsd-control -c ${nsdControlConfig}";
            checkzone = "${pkgs.nsd}/bin/nsd-checkzone";
            cat = "${pkgs.coreutils}/bin/cat";
            touch = "${pkgs.coreutils}/bin/touch";
            stat = "${pkgs.coreutils}/bin/stat";
            inherit baseDir;
          }}", NULL, NULL
        };

        if (argc != 2) {
          fprintf(stderr, "Usage: %s FQDN\n", argv[0]);
          fputs("  Zone data is read from stdin.\n", stderr);
          return EXIT_FAILURE;
        }

        if ((pw = getpwnam("dynzone")) == NULL) {
          perror("getpwnam(\"dynzone\")");
          return EXIT_FAILURE;
        }

        if ((curuid = geteuid()) != pw->pw_uid) {
          fprintf(stderr, "fatal: Expected uid %d but got uid %d.\n",
                  pw->pw_uid, curuid);
          return EXIT_FAILURE;
        }

        execargv[5] = argv[1];
        execve("${pkgs.stdenv.shell}", execargv, emptyenv);
        return EXIT_SUCCESS;
      }
    '';
  };

in {
  options.headcounter.nsd-zone-writer = {
    enable = lib.mkEnableOption "NSD zone writer";

    beforeUnits = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      description = ''
        The list of units that need to wait until the zone writer directory
        has been set up.
      '';
    };

    command = lib.mkOption {
      type = lib.types.path;
      internal = true;
      description = ''
        The path to the actual writer command, useful for other modules that may
        want to use the nsd-zone-writer.
      '';
    };

    allowedUsers = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      description = ''
        A list of users allowed to update zone files.
      '';
    };
  };

  config = lib.mkIf config.headcounter.nsd-zone-writer.enable {
    systemd.services.nsd-setup-dynzones = {
      description = "Create NSD Zone Directory For Dynamic Zone Writer";
      requiredBy = [ "nsd.service" ];
      after = [ "nsd.service" ];
      before = config.headcounter.nsd-zone-writer.beforeUnits;
      serviceConfig.Type = "oneshot";
      serviceConfig.UMask = "0057";
      script = ''
        chgrp dynzone ${lib.escapeShellArg nsdStateDir}
        chmod g=x ${lib.escapeShellArg nsdStateDir}

        mkdir -p ${lib.escapeShellArg baseDir}
        chown dynzone:nsd ${lib.escapeShellArg baseDir}
        chmod g=sx ${lib.escapeShellArg baseDir}
      '';
    };

    users.users.dynzone = {
      description = "User for Updating Dynamic NSD Zones";
      uid = 2023;
    };

    users.groups.dynzone = {
      gid = 2023;
      members = config.headcounter.nsd-zone-writer.allowedUsers;
    };

    services.nsd = {
      enable = setOpt true;
      xfrdReloadTimeout = setOpt 0;

      extraConfig = ''
        pattern:
          name: "dynzone"
          zonefile: "${baseDir}/%s.zone"
      '';

      remoteControl = let
        # We can use this as long as remoteControl.interfaces doesn't serve on
        # external interfaces.
        snakeOil = pkgs.runCommand "nsd-control-certs" {
          buildInputs = [ pkgs.openssl ];
        } ''
          mkdir -p "$out"
          "${pkgs.nsd}/bin/nsd-control-setup" -d "$out"
        '';
      in lib.mkMerge [
        { interfaces = setOpt [ "127.0.0.1" ]; }
        (lib.mkIf (nsdcfg.interfaces == [ "127.0.0.1" ]) {
          enable = setOpt true;
          controlKeyFile = setOpt "${snakeOil}/nsd_control.key";
          controlCertFile = setOpt "${snakeOil}/nsd_control.pem";
          serverKeyFile = setOpt "${snakeOil}/nsd_server.key";
          serverCertFile = setOpt "${snakeOil}/nsd_server.pem";
        })
      ];
    };

    systemd.mounts = lib.singleton {
      description = "Zone Writer Commands Directory";
      what = "tmpfs";
      where = "/run/nsd-zone-writer";
      type = "tmpfs";
      options = "nodev";
    };

    systemd.automounts = lib.singleton {
      wantedBy = [ "local-fs.target" ];
      where = "/run/nsd-zone-writer";
      requires = [ "setup-zone-writer-commands.service" ];
    };

    systemd.services.setup-zone-writer-commands = {
      description = "Set Up NSD Zone Writer Commands";
      after = [ "nsd-zone-writer.mount" ];
      serviceConfig.Type = "oneshot";
      serviceConfig.RemainAfterExit = true;

      script = ''
        install -m 6050 -o dynzone -g dynzone \
          ${lib.escapeShellArg zoneWriter} /run/nsd-zone-writer/write
      '';
    };

    headcounter.nsd-zone-writer.command = "/run/nsd-zone-writer/write";
  };
}
