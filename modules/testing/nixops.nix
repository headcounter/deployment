# Pluggable module which accepts common NixOps options so that it can be used
# for VM tests on NixOps configurations.
#
# At the moment, this module mainly is for supporting encryptedLinksTo and a few
# other options and thus is by no means exhaustive.
{ pkgs, nodes, config, lib, ... }:

let
  inherit (lib) types mkOption;

  ignoreOption = mkOption {
    type = types.undefined;
    internal = true;
    description = ''
      Sink to declare the option but not use its value for anything, because
      it's highly NixOps specific.
    '';
  };

  nixopsSource = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixops";
    rev = "812e33201760e47a663de973e9501b10fe67356f";
    sha256 = "1m3vijmwccm5wqyl1hrgxfvi7ynv9wgrwag52vlnscm5igl2hlzn";
  };

  keypairType = types.submodule {
    options.privateKey = mkOption {
      type = types.str;
      description = "The SSH private key for this pair.";
    };
    options.publicKey = mkOption {
      type = types.str;
      description = "The SSH public key for this pair.";
    };
  };

  mkKeyPair = name: import (pkgs.runCommand "snakeoil-keypair-${name}.nix" {
    buildInputs = [ pkgs.openssh ];
    keyComment = "snakeoil-key-${name}";
  } ''
    ssh-keygen -t ed25519 -f key -N "" -C "$keyComment"
    transform='$! { :l; N; $!bl; }; s/["\\$]/\\&/g; s/\n/\\n/g; p'
    ( echo "{"
      echo "  privateKey = \"$(sed -n -e "$transform" key)\\n\";"
      echo "  publicKey = \"$(sed -n -e "$transform" key.pub)\";"
      echo "}"
    ) > "$out"
  '');

in {
  imports = [ "${nixopsSource}/nix/ssh-tunnel.nix" ];

  options.headcounter.nixops.snakeoilKeypair = mkOption {
    type = keypairType;
    internal = true;
    description = "The snakeoil VPN keypair for this node.";
  };

  options.headcounter.nixops.resources.sshKeyPairs = mkOption {
    type = types.attrsOf keypairType;
    default = {};
    description = ''
      SSH keyairs to be passed from the deployment, generated and passed back
      to the module arguments.
    '';
  };

  options.deployment = {
    targetEnv = ignoreOption;
    keys = ignoreOption; # TODO!
    storeKeysOnMachine = ignoreOption;
    container = ignoreOption;

    hetzner = {
      mainIPv4 = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Main IP address identifying the machine.";
      };
      robotUser = ignoreOption;
      robotPass = ignoreOption;
      partitions = ignoreOption;
    };

    encryptedLinksTo = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Encrypted tunnels to set up.";
    };
  };

  options.networking = {
    privateIPv4 = mkOption {
      type = types.str;
      description = "Private IPv4 address of this machine.";
    };

    publicIPv4 = mkOption {
      type = types.str;
      description = "Public IPv4 address of this machine.";
    };
  };

  config = let
    isMyself = nodeName: let
      nodeHostName = nodes.${nodeName}.config.networking.hostName;
    in nodeHostName == config.networking.hostName;

    getLinks = node: attrs: let
      others = attrs.config.deployment.encryptedLinksTo or [];
      mkPair = other: let
        otherLinks = nodes.${other}.config.deplopment.encryptedLinksTo or [];
        conflicts = lib.elem node otherLinks;
        isClient = !(conflicts && node < other);
      in if isClient then [ node other ] else null;
    in lib.filter (x: x != null) (map mkPair others);

    links = lib.unique (lib.concatLists (lib.mapAttrsToList getLinks nodes));

    privateIps = let
      mod = a: b: a - b * (a / b);
      # Nix implementation of index_to_private_ip() in deployment.py:
      mkIp = index: let
        x = 105 + index / 256;
        y = mod index 256;
      in assert x <= 255; {
        ip = "192.168.${toString x}.${toString y}";
        tunnel = 10000 + index;
      };
      mkResult = index: node: lib.nameValuePair node (mkIp (index - 1));
    in lib.listToAttrs (lib.imap mkResult (lib.attrNames nodes));

    addrAssignments = map (link: let
      client = lib.head link;
      server = lib.last link;
      localIPv4 = privateIps.${client}.ip;
      remoteIPv4 = privateIps.${server}.ip;
      localTunnel = privateIps.${server}.tunnel;
      remoteTunnel = privateIps.${client}.tunnel;
      inherit (nodes.${client}.config.headcounter.nixops) snakeoilKeypair;
    in lib.optionalAttrs (isMyself client) {
      tunnelConf.${server} = {
        target = "${server}-unencrypted";
        targetPort = 22;
        inherit localIPv4 remoteIPv4 localTunnel remoteTunnel;
        privateKey = "/etc/nixops/id_snakeoil";
      };
      trustedInterfaces = [ "tun${toString localTunnel}" ];
      kernelModules = [ "tun" ];
      hosts.${remoteIPv4} = [ server "${server}-encrypted" ];
    } // lib.optionalAttrs (isMyself server) {
      inherit (snakeoilKeypair) publicKey;
      trustedInterfaces = [ "tun${toString remoteTunnel}" ];
      kernelModules = [ "tun" ];
      hosts.${localIPv4} = [ client "${client}-encrypted" ];
    }) links;

    allHosts = let
      tunnelHosts = let
        hosts = lib.catAttrs "hosts" addrAssignments;
      in lib.zipAttrsWith (lib.const lib.concatLists) hosts;

      mkNodeHosts = node: attrs: {
        name = attrs.config.networking.primaryIPAddress;
        value = [ node "${node}-unencrypted" ];
      };

      nodeHosts = let
        others = lib.filterAttrs (node: lib.const (!isMyself node)) nodes;
      in lib.mapAttrs' mkNodeHosts others;

      localHosts."127.0.0.1" = [ "${config.networking.hostName}-encrypted" ];

    in lib.zipAttrsWith (lib.const lib.concatLists) [
      tunnelHosts nodeHosts localHosts
    ];

  in {
    boot.kernelModules = let
      mods = lib.catAttrs "kernelModules" addrAssignments;
    in lib.concatLists mods;

    networking.firewall.trustedInterfaces = let
      ifaces = lib.catAttrs "trustedInterfaces" addrAssignments;
    in lib.concatLists ifaces;

    networking.p2pTunnels.ssh = let
      tunnels = lib.catAttrs "tunnelConf" addrAssignments;
    in lib.zipAttrsWith (lib.const lib.head) tunnels;

    services.openssh.enable = true;
    services.openssh.extraConfig = "PermitTunnel yes";

    users.users.root.openssh.authorizedKeys = {
      keys = lib.catAttrs "publicKey" addrAssignments;
    };

    networking.extraHosts = let
      mkEntry = addr: names: {
        key = lib.head names;
        value = "${addr} ${lib.concatStringsSep " " names}";
      };
      hostList = lib.mapAttrsToList mkEntry allHosts;
      sorter = a: b: a.key <= b.key;
      sorted = lib.catAttrs "value" (lib.sort sorter hostList);
    in lib.concatStringsSep "\n" sorted;

    networking.privateIPv4 = config.networking.primaryIPAddress;
    networking.publicIPv4 = config.networking.primaryIPAddress;

    headcounter.nixops.snakeoilKeypair = let
      name = "vpn-${config.networking.hostName}";
    in mkKeyPair name;

    environment.etc."nixops/id_snakeoil" = {
      text = config.headcounter.nixops.snakeoilKeypair.privateKey;
      mode = "0600";
    };

    _module.args.resources = {
      sshKeyPairs = let
        inherit (config.headcounter.nixops.resources) sshKeyPairs;
      in lib.mapAttrs (lib.flip (lib.const mkKeyPair)) sshKeyPairs;
    };
  };
}
