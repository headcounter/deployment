{ config, hclib, lib, ... }:

let
  epmdAddresses = config.headcounter.services.epmd.addresses;
  isLoopbackEpmd = epmdAddresses == lib.singleton "127.0.0.1";
  distUseLoopback = "-kernel inet_dist_use_interface '{127, 0, 0, 1}'";

  mimcfg = config.headcounter.services.mongooseim;
  needsInet = mimcfg.enable && mimcfg.nodeIp != null;
  inherit (config.headcounter.erlang-inet) inetConfigFile;
  inherit (hclib) shErlEsc erlString;
  kernelInetrc = "-kernel inetrc ${shErlEsc erlString inetConfigFile}";

in {
  loopbackArg = lib.optionalString isLoopbackEpmd distUseLoopback;
  inetArg = lib.optionalString needsInet kernelInetrc;
}
