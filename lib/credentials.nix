lib:

rec {
  hasCredentials = builtins.pathExists ../credentials.nix;

  getcred = sym: default: let
    path = if builtins.isString sym then lib.singleton sym else sym;
    pathStr = lib.concatStringsSep "." path;
    suicide = throw "Can't find attribute path `${pathStr}' in credentials.";
    creds = import ../credentials.nix;
  in if hasCredentials then lib.attrByPath path suicide creds else default;
}
