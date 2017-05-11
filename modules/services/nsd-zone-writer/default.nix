{ config, pkgs, lib, ... }:

let
  setOpt = lib.mkOverride 900;

  nsdcfg = config.services.nsd.remoteControl;

  ctlKeyDir = "/var/lib/nsd-zone-writer-ctl";
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
      #include <string.h>
      #include <sys/types.h>
      #include <unistd.h>

      int main(int argc, char *argv[]) {
        uid_t curuid;
        struct passwd *pw;
        const char *fqdn;
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
            rm = "${pkgs.coreutils}/bin/rm";
            inherit baseDir;
          }}", NULL, NULL, NULL
        };

        if (argc == 3 && strncmp(argv[1], "--delete", 9) == 0) {
          execargv[5] = "--delete";
          fqdn = execargv[6] = argv[2];
        } else if (argc == 2) {
          fqdn = execargv[5] = argv[1];
        } else {
          fprintf(stderr, "Usage: %s [--delete] FQDN\n", argv[0]);
          fputs("  Zone data is read from stdin.\n", stderr);
          return EXIT_FAILURE;
        }

        while (*fqdn != '\0') {
          if (
            (*fqdn >= 'a' && *fqdn <= 'z') ||
            (*fqdn >= 'A' && *fqdn <= 'Z') ||
            (*fqdn >= '0' && *fqdn <= '9') ||
            *fqdn == '.' || *fqdn == '-' ||
            *fqdn == '_'
          ) {
            fqdn++;
            continue;
          }
          fprintf(stderr, "Invalid character '%c' in FQDN.\n", *fqdn);
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
      serviceConfig.RemainAfterExit = true;
      serviceConfig.UMask = "0057";
      script = ''
        chgrp dynzone ${lib.escapeShellArg nsdStateDir}
        chmod g=x ${lib.escapeShellArg nsdStateDir}

        mkdir -p ${lib.escapeShellArg baseDir}
        chown dynzone:nsd ${lib.escapeShellArg baseDir}
        chmod g=sx ${lib.escapeShellArg baseDir}
      '';
    };

    systemd.services.nsd-control-keys-setup = let
      keyFiles = map (f: "${ctlKeyDir}/${f}") [
        "nsd_control.key" "nsd_control.pem" "nsd_server.key" "nsd_server.pem"
      ];
    in {
      description = "Set Up NSD Control Keys";
      requiredBy = [ "nsd.service" ];
      before = [ "nsd.service" ];
      serviceConfig.Type = "oneshot";
      serviceConfig.RemainAfterExit = true;
      serviceConfig.UMask = "0077";
      serviceConfig.User = "dynzonekeys";
      serviceConfig.Group = "dynzone";
      serviceConfig.PermissionsStartOnly = true;
      unitConfig.ConditionPathExists = map (f: "!${f}") keyFiles;
      path = [ pkgs.openssl ];
      preStart = ''
        mkdir -p ${lib.escapeShellArg ctlKeyDir}
        chown dynzonekeys ${lib.escapeShellArg ctlKeyDir}
      '';
      script = ''
        "${pkgs.nsd}/bin/nsd-control-setup" -d ${lib.escapeShellArg ctlKeyDir}
      '';
      postStart = ''
        chown root ${lib.escapeShellArg ctlKeyDir}
        ${lib.concatMapStrings (keyFile: ''
          chmod 0440 ${lib.escapeShellArg keyFile}
          chown dynzone:nsd ${lib.escapeShellArg keyFile}
        '') keyFiles}
        chmod o+x ${lib.escapeShellArg ctlKeyDir}
      '';
    };

    users.users.dynzonekeys = {
      description = "User for Creating NSD Control Keys";
      uid = 2024;
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

      remoteControl = lib.mkMerge [
        { interfaces = setOpt [ "127.0.0.1" ]; }
        (lib.mkIf (nsdcfg.interfaces == [ "127.0.0.1" ]) {
          enable = setOpt true;
          controlKeyFile = setOpt "${ctlKeyDir}/nsd_control.key";
          controlCertFile = setOpt "${ctlKeyDir}/nsd_control.pem";
          serverKeyFile = setOpt "${ctlKeyDir}/nsd_server.key";
          serverCertFile = setOpt "${ctlKeyDir}/nsd_server.pem";
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
