# Support module which should simulate a virtual Internet-like environment.
#
# This doesn't involve adding any routers but instead rewrites the network
# interface address configuration for the test nodes.
#
# XXX: Right now this module isn't pluggable and heavily depends on
#      Headcounter-internal options (search for "config.headcounter"), because
#      we can't simply replace the interface options created by the "ip-address"
#      module key in 17.03.
#
#      However, NixOS unstable (and upcoming 17.09) has a "disabledModules"
#      attribute which we can use to replace the "ip-address" module.
#
{ nodes, config, lib, ... }:

let
  # We do our address assignment by ourselves, because we want to have a more
  # unrestricted prefix length in our network.
  forceSingleton = x: lib.mkForce (lib.singleton x);
  maybeForce = cond: x: lib.optionals (cond == null) (forceSingleton x);
  getIndex = e: list:
    if list == [] then throw "Node index for ${e} not found!"
    else if lib.head list == e then lib.length list
    else getIndex e (lib.tail list);
  index = getIndex config.networking.hostName (lib.attrNames nodes);

  # TODO: Move into lib/ maybe?
  hexChars = lib.stringToCharacters "0123456789abcdef";
  dec2hex = x: let
    d = x / 16;
    r = x - d * 16;
    hex = builtins.elemAt hexChars r;
  in if d == 0 then hex else dec2hex d + hex;

  # Either use the IP address provided in opt or if that is null, use fallback.
  mkAddr = opt: fallback:
    if config.headcounter.${opt} != null
    then config.headcounter.${opt}
    else fallback;

  mainIPv4.address = mkAddr "mainIPv4" "93.184.216.${toString index}";
  mainIPv4.prefixLength = 0;
  mainIPv6.address = mkAddr "mainIPv6" "fdbc::${dec2hex index}";
  mainIPv6.prefixLength = 0;

  vhostCfg = let
    inherit (config.headcounter) internalNetConfig mainDevice;
  in internalNetConfig.${mainDevice} or { ip4 = []; ip6 = []; };

in {
  virtualisation.vlans = lib.mkForce [ 1 ];
  headcounter.mainDevice = "eth1";

  networking.useDHCP = false;
  networking.primaryIPAddress = lib.mkForce mainIPv4.address;

  # Force-override the IP addresses of the interface so that we don't get the
  # adresses assigned by the NixOS testing system.
  networking.interfaces.${config.headcounter.mainDevice} = {
    ip4 = lib.mkForce (lib.singleton mainIPv4 ++ vhostCfg.ip4);
    ip6 = lib.mkForce (lib.singleton mainIPv6 ++ vhostCfg.ip6);
  };

  networking.defaultGateway = {
    address = "0.0.0.0";
    interface = "eth1";
  };
}
