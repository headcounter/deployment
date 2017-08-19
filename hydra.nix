{ pkgs, lib, config, nodes, resources, ... }:

with lib;

let
  pkgsReimport = import <nixpkgs> {};

  hydraSrc = pkgsReimport.stdenv.mkDerivation rec {
    name = "hydra-source-${toString revCount}";

    rev = "337a72ef4dc827848e073339da01e49ffbff1755";
    revCount = 2455;

    src = pkgsReimport.fetchFromGitHub {
      repo = "hydra";
      owner = "NixOS";
      inherit rev;
      sha256 = "06i9q9z748g8adbls651n4bjfila81c5m6av921d2mzhq7rjq17s";
    };

    phases = [ "unpackPhase" "patchPhase" "installPhase" ];

    postPatch = let
      betterBoehm = "(boehmgc.override { enableLargeConfig = true; })";
    in ''
      # Make sure Hydra uses boehm with enableLargeConfig
      sed -i -e '/nix = .*;/ {
        c nix = nixUnstable.override { boehmgc = ${betterBoehm}; };
      }' -e 's/boehmgc/${betterBoehm}/g' release.nix

      # Workaround for poor-mans XSRF protection only taking into account
      # Hydras running on / but not subpaths.
      sed -i -e '/#.*XSRF protection/ {
        :l; n; /}/b
        s!^\( *my *\$base * =\).*!\1 '\'''https://headcounter.org/'\''';!
        /die\>.*\$base/d
        bl
      }' src/lib/Hydra/Controller/Root.pm
    '';

    installPhase = "cp -r . $out";
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

  services.hydra-dev = {
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
  }) ++ [
    { hostName = "falayalaralfali";
      maxJobs = 8;
      systems = [ "armv6l-linux" "armv7l-linux" ];
      sshKey = "/run/keys/buildkey.priv";
      sshUser = buildUser;
      supportedFeatures = [ "kvm" "nixos-test" "big-parallel" ];
    }
  ];

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
