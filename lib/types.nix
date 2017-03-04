lib:

{
  types.port = lib.mkOptionType {
    name = "TCP port";
    check = p: lib.isInt p && p <= 65535 && p >= 0;
    merge = lib.mergeOneOption;
  };
}
