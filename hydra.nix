{ pkgs, lib, config, nodes, resources, ... }:

with lib;

let
  hydraSrc = (import <nixpkgs> {}).fetchFromGitHub {
    repo = "hydra";
    owner = "NixOS";
    rev = "53c80d9526fb029b7adde47d0cfaa39a80926c48";
    sha256 = "095zvi1pbcxr395ss44c399vmpp5z422lvm0iwjpkia19nr96zd5";
  };

  hydraRelease = import "${hydraSrc}/release.nix" {
    inherit hydraSrc;
    officialRelease = true;
  };

  hydraModule = import "${hydraSrc}/hydra-module.nix";

  hydra = builtins.getAttr config.nixpkgs.system hydraRelease.build;

  buildUser = "hydrabuild";

  isBuildNode = name: node: hasAttr buildUser node.config.users.extraUsers;
  buildNodes = filterAttrs isBuildNode nodes;

  buildKey = resources.sshKeyPairs."hydra-build".privateKey;
in {
  imports = singleton hydraModule;

  services.hydra = {
    package = hydra;
    enable = true;
    hydraURL = "https://headcounter.org/hydra/";
    notificationSender = "hydra@headcounter.org";
    listenHost = "localhost";
    extraConfig = ''
      binary_cache_secret_key_file = /run/keys/binary-cache.secret
    '';
  };

  users.extraUsers.hydra-www.extraGroups = [ "keys" ];
  users.extraUsers.hydra-queue-runner.extraGroups = [ "keys" ];
  systemd.services.hydra-init.requires = [ "keys.target" ];
  systemd.services.hydra-init.after = [ "keys.target" ];

  nix.distributedBuilds = true;
  nix.buildMachines = flip mapAttrsToList buildNodes (hostName: node: {
    inherit hostName;
    inherit (node.config.nix) maxJobs;
    systems = if node.config.nixpkgs.system == "x86_64-linux"
              then [ "i686-linux" "x86_64-linux" ]
              else singleton node.config.nixpkgs.system;
    sshKey = "/run/keys/buildkey.priv";
    sshUser = buildUser;
    supportedFeatures = [ "kvm" "nixos-test" "big-parallel" ];
  });

  nix.extraOptions = ''
    gc-keep-outputs = true
    gc-keep-derivations = true
  '';

  deployment.keys."binary-cache.secret" = {
    text = (import ./ssl/hydra.nix).secret;
    user = "hydra-www";
    permissions = "0400";
  };

  deployment.keys."buildkey.priv" = {
    text = buildKey;
    user = "hydra-queue-runner";
    permissions = "0400";
  };

  deployment.keys."signkey.priv".text = readFile ./ssl/signing-key.sec;
  deployment.storeKeysOnMachine = false;

  environment.etc."nix/signing-key.sec".source = "/run/keys/signkey.priv";
}
