{ pkgs, lib, ... }: let
  inherit (import ../mongooseim/lib.nix {
    inherit pkgs lib;
  }) runInCtl checkListeners;
in {
  name = "listeners";
  testScript = { nodes, ... }:
    runInCtl "ultron" (checkListeners nodes.ultron);
}
