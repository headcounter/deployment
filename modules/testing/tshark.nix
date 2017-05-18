# Pluggable module for VM tests which attaches a sniffer on all network
# interfaces (including the loopback interface).
#
# NOTE: Do *NOT* use this in production *ever*, because the sniffers are
#       running as root user and increase the attack surface substantially.
{ pkgs, ... }:

{
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="net", KERNEL!="lo", TAG+="systemd", \
      ENV{SYSTEMD_WANTS}="tshark@%k.service"
  '';

  systemd.services."check-if-up@" = {
    description = "Wait For Network Interface %I To Become Available";
    serviceConfig.ExecStart = "${pkgs.writeScript "check-if-up" ''
      #!${pkgs.stdenv.shell}
      while ${pkgs.coreutils}/bin/sleep 0.1; do
        [ "$(< "/sys/class/net/$1/carrier")" = 1 ] && break
      done
    ''} %I";
    serviceConfig.Type = "oneshot";
  };

  systemd.services.tshark-lo = {
    description = "Attach Sniffer To Loopback Device";
    wantedBy = [ "multi-user.target" ];
    requires = [ "check-if-up@lo.service" ];
    after = [ "check-if-up@lo.service" ];
    serviceConfig = {
      SyslogIdentifier = "tshark-loopback";
      ExecStart = "${pkgs.wireshark}/bin/tshark -n -i lo";
    };
  };

  systemd.services."tshark@" = {
    description = "Attach Sniffer To Network Device %I";
    bindsTo = [ "sys-subsystem-net-devices-%i.device" ];
    requires = [ "check-if-up@%i.service" ];
    after = [
      "sys-subsystem-net-devices-%i.device"
      "check-if-up@%i.service"
    ];
    serviceConfig = {
      SyslogIdentifier = "tshark-%I";
      ExecStart = "${pkgs.wireshark}/bin/tshark -n -i %I";
    };
  };
}
