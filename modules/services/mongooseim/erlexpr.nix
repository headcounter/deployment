with import <nixpkgs> {};
with pkgs.lib;

rec {
  erlAtom = val: let
    numbers = stringToCharacters "0123456789";
    allowed = lowerChars ++ upperChars ++ numbers ++ ["@" "_"];
    inRange = range: chr: any (c: c == chr) range;
    needsEscape = chrList: !inRange lowerChars (head chrList) ||
                           !all (inRange allowed) chrList;
  in if needsEscape (stringToCharacters val)
     then "'${escape ["'"] val}'"
     else val;

  erlString = val: "\"${escape ["\""] (toString val)}\"";
  erlInt = toString;
  erlBool = val: erlAtom (if val then "true" else "false");
  erlList = val: "[${concatStringsSep ", " (map toErl val)}]";
  erlTuple = val: "{${concatStringsSep ", " (map toErl val)}}";

  erlPropList = val: let
    mkExTuple = name: extension: erlTuple ([{ atom = name; }] ++ extension);
    mkTuple = name: value:
      if value ? flag
      then (if value.flag then erlAtom name else null)
      else if value ? extuple then mkExTuple name value.extuple
      else "{${erlAtom name}, ${toErl value}}";
    tuples = remove null (mapAttrsToList mkTuple val);
  in "[${concatStringsSep ", " tuples}]";

  toErl = val: let
    nonNix = attr:
      if attr ? __raw then attr.__raw
      else if attr ? atom then erlAtom attr.atom
      else if attr ? tuple then erlTuple attr.tuple
      else erlPropList attr;
    nix = term:
      if builtins.isInt term then toString term
      else if builtins.isBool term then erlBool term
      else if builtins.isString term then erlString term
      else if isList term then erlList term
      else throw "Can't transform ${traceVal term} into an Erlang expression!";
  in (if isAttrs val then nonNix else nix) val;

  erlType = valType: mkOptionType {
    name = "an Erlang expression";
    merge = loc: defs: valType (mergeOneOption loc defs);
  };

  parseErlIpAddr = addr: import "${pkgs.runCommand "erlip.nix" {} ''
    ${pkgs.erlang}/bin/erl -noshell -eval '
      {ok, Addr} = inet:parse_address(${erlString addr}),
      io:fwrite("\"~p\"", [Addr])
    ' -s erlang halt > "$out"
  ''}";

  shErlEsc = escaper: str: let
    doubleSlashed = escape ["\\"] (escaper str);
    shQuoted = replaceChars ["'"] [("'\\'" + "'")] doubleSlashed;
  in "'${shQuoted}'";
}
