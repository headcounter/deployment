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
    zoneCommand = toString (pkgs.writeScript "write-zone" ''
      #!${pkgs.stdenv.shell} -e
      fqdn="$1"
      umask 0007
      zonefile="/var/lib/nsd/dynzones/$fqdn.zone"

      touchZonefile() {
        touch -r "$zonefile" -d '1 sec' "$zonefile"
      }

      ctrl() {
        "${pkgs.nsd}/bin/nsd-control" -c "${nsdControlConfig}" "$@"
      }

      if [ -e "$zonefile" ]; then
        oldMTime="$(stat -c %Y "$zonefile")"
        exists=1
      else
        oldMTime=0
        exists=0
      fi
      cat > "$zonefile"
      if [ "$oldMTime" -eq "$(stat -c %Y "$zonefile")" ]; then
        touchZonefile
      fi
      coproc waitForUpdate {
        "${pkgs.inotify-tools}/bin/inotifywait" \
          --format %w -m -e close "$zonefile" 2>&1
      }
      watching=0
      while read line <&''${waitForUpdate[0]}; do
        if [ "x$line" = "xWatches established." ]; then
          watching=1
          break
        fi
      done
      if [ $watching -eq 0 ]; then
        kill -TERM %% &> /dev/null || :
        echo "Could not establish inotify watch for $zonefile!" >&2
        exit 1
      fi
      if [ $exists -eq 1 ]; then
        echo -n "Reloading zone $fqdn: " >&2
        ctrl reload "$fqdn" >&2
      else
        ctrl addzone "$fqdn" dyndns
      fi

      for waitTime in 1 2 5 10 30; do
        if read -t $waitTime line <&''${waitForUpdate[0]}; then
          if [ "x$line" = "x$zonefile" ]; then
            kill -TERM %% &> /dev/null || :
            wait &> /dev/null || :
            echo "Reload of $fqdn successful." >&2
            exit 0
          fi
        fi
        echo "Reload of $fqdn failed, touching zone file and" \
             "resending reload..." >&2
        touchZonefile
        read touched <&''${waitForUpdate[0]}
        ctrl reload "$fqdn" >&2
      done
      echo "Reloading of zone $fqdn failed after 5 retries." >&2
      exit 1
    '');
  };
}
