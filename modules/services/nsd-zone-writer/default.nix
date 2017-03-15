{ config, pkgs, lib, ... }:

let
  setOpt = lib.mkOverride 900;

  nsdcfg = config.services.nsd.remoteControl;

  nsdControlConfig = pkgs.writeText "nsd-control.config" ''
    remote-control:
      control-enable:    yes
      control-key-file:  "${nsdcfg.controlKeyFile}"
      control-cert-file: "${nsdcfg.controlCertFile}"
      control-port:      ${toString nsdcfg.port}
      server-cert-file:  "${nsdcfg.serverCertFile}"
  '';

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
  };

  config = lib.mkIf config.headcounter.nsd-zone-writer.enable {
    systemd.services.nsd-setup-dynzones = {
      description = "Create NSD Zone Directory For Dynamic Zone Writer";
      requiredBy = [ "nsd.service" ];
      after = [ "nsd.service" ];
      before = config.headcounter.nsd-zone-writer.beforeUnits;
      serviceConfig.Type = "oneshot";
      serviceConfig.UMask = "0007";
      script = ''
        mkdir -p /var/lib/nsd/dynzones
        # XXX: This needs to be configurable:
        chown dyndns:nsd /var/lib/nsd/dynzones
        chmod g+s /var/lib/nsd/dynzones
      '';
    };

    services.nsd = {
      enable = setOpt true;
      xfrdReloadTimeout = setOpt 0;

      extraConfig = ''
        pattern:
          name: "dynzone"
          zonefile: "/var/lib/nsd/dynzones/%s.zone"
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

    system.build.nsd-zone-writer = pkgs.substituteAll {
      name = "nsd-write-zone";
      src = ./writer.sh.in;
      isExecutable = true;
      inotifywait = "${pkgs.inotify-tools}/bin/inotifywait";
      ctrl = "${pkgs.nsd}/bin/nsd-control -c ${nsdControlConfig}";
    };
  };
}
