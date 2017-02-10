{ lib ? import <nixpkgs/lib> }:

import ./erlang lib //
import ./module-support.nix lib //
import ./credentials.nix lib
