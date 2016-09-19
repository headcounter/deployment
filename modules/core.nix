{ lib, ... }:

{
  _module.args.hclib = import ../lib { inherit lib; };
}
