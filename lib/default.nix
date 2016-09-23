{ lib ? import <nixpkgs/lib> }:

import ./erlang.nix lib //
import ./module-support.nix lib
