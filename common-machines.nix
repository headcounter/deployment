{ pkgs, lib, config, ... }:

let
  vuizvui = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "openlab-aux";
    repo = "vuizvui";
    rev = "9e235ef0f0d48f241ec6b0bcb7b332c182c2aadf";
    sha256 = "1ja2nc3q0c4il6f507mlxig5fzl3j384asnx3jb89r7jjryjm0jr";
  };

  vim = "${vuizvui}/modules/user/aszlig/programs/vim/default.nix";

in {
  imports = [ ./common.nix vim ];

  vuizvui.user.aszlig.programs.vim.enable = true;

  deployment.targetEnv = lib.mkOverride 900 "hetzner";

  environment.systemPackages = with pkgs; [
    atop htop iotop
    sysstat dstat
    smartmontools
    perf-tools
    netrw
  ];

  services.openntpd.enable = true;

  nix = {
    package = pkgs.nixUnstable;
    nrBuildUsers = 100;
    useChroot = true;
    readOnlyStore = true;
    buildCores = 0;
  };

  nixpkgs.config.allowUnfree = true;
  hardware.cpu.intel.updateMicrocode = true;

  time.timeZone = "Europe/Berlin";
}
