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
  systemd.services.dyndns-slave.after = [ "nsd.service" ];

  systemd.services.dyndns-create-zonedir = {
    description = "Create NSD Zone Directory For Dynamic DNS Slave";
    requiredBy = [ "nsd.service" ];
    after = [ "nsd.service" ];
    before = [ "dyndns-slave.service" ];
    serviceConfig.Type = "oneshot";
    serviceConfig.UMask = "0007";
    script = ''
      mkdir -p /var/lib/nsd/dynzones
      chown dyndns:nsd /var/lib/nsd/dynzones
      chmod g+s /var/lib/nsd/dynzones
    '';
  };

  services.nsd = {
    enable = setOpt true;
    xfrdReloadTimeout = setOpt 0;

    extraConfig = ''
      # XXX for <nixpkgs> before 8442a7d12c399cc8bbe6cd6c4092b0df9f55dbac
      remote-control:
        control-port: ${toString nsdcfg.port}

      pattern:
        name: "dyndns"
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

  headcounter.services.dyndns.slave = {
    zoneCommand = setOpt (toString (pkgs.substituteAll {
      name = "nsd-write-zone";
      src = ./nsd-write-zone.sh.in;
      isExecutable = true;
      inotifywait = "${pkgs.inotify-tools}/bin/inotifywait";
      ctrl = "${pkgs.nsd}/bin/nsd-control -c ${nsdControlConfig}";
    }));
  };
}
