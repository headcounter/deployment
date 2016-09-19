lib:

let
  inherit (lib) stringToCharacters lowerChars upperChars numbers escape;
  inherit (lib) head tail any all range remove mapAttrsToList concatStringsSep;
  inherit (lib) isList isAttrs traceVal replaceStrings splitString last toInt;
  inherit (lib) init take drop genList const length foldl' toLower zipLists;
  inherit (lib) findFirst mkOptionType mergeOneOption;

in rec {
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

  parseErlIpAddr = addr: let
    throwInvalid = throw "Invalid IPv4 or IPv6 address: ${addr}";
    tuplize = val: "{${concatStringsSep ", " val}}";

    v4DigitRe = "(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])";
    v6DigitRe = "([0-9a-fA-F]{1,4})";
    v4Re = "${v4DigitRe}\.${v4DigitRe}\.${v4DigitRe}\.${v4DigitRe}";
    v4Parse = builtins.match v4Re addr;

    v6Expanded = let
      splitted = let
        simple = splitString ":" addr;
        v4mapped = builtins.match v4Re (last simple);
        rewritten = init simple ++ [ (take 2 v4mapped) (drop 2 v4mapped) ];
      in if v4mapped != null then rewritten else simple;
      folder = acc: digit: let
        pad = genList (const "0") (8 - (length splitted - length acc));
      in if digit == "" then acc ++ pad else acc ++ [ digit ];
      result = foldl' folder [] splitted;
    in if length result != 8 then throwInvalid else result;

    v6Parse = let
      digitizeV6 = digit: let
        chars = stringToCharacters (toLower digit);
        padded = genList (const "0") (4 - length chars) ++ chars;
        table = zipLists (stringToCharacters "0123456789abcdef") (range 0 15);
        toDec = c: (findFirst (conv: conv.fst == c) throwInvalid table).snd;
        d1 = toDec (head padded);
        d2 = toDec (head (tail padded));
        d3 = toDec (last (init padded));
        d4 = toDec (last padded);
      in toString (d1 * 4096 + d2 * 256 + d3 * 16 + d4);

      digitizeV4 = d: toString (toInt (head d) * 256 + toInt (last d));

      convert = digit: let
        result = builtins.match v6DigitRe digit;
        v6 = if result != null then digitizeV6 (head result) else throwInvalid;
      in if isList digit then digitizeV4 digit else v6;
    in tuplize (map convert v6Expanded);

  in if v4Parse != null then tuplize v4Parse else v6Parse;

  shErlEsc = escaper: str: let
    doubleSlashed = escape ["\\"] (escaper str);
    shQuoted = replaceStrings ["'"] [("'\\'" + "'")] doubleSlashed;
  in "'${shQuoted}'";
}
