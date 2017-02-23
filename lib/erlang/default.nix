lib:

let
  inherit (lib) stringToCharacters lowerChars upperChars numbers escape toInt;
  inherit (lib) head tail any all range mapAttrsToList concatStringsSep filter;
  inherit (lib) isList isAttrs replaceStrings toLower last optional singleton;
  inherit (lib) init take drop genList const length foldl' zipLists findFirst;
  inherit (lib) mkOptionType mergeOneOption;

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

  erlString = val: "\"${import ./escape-string.nix (toString val)}\"";
  erlBinary = val: "<<${erlString val}>>";
  erlInt = toString;
  erlBool = val: erlAtom (if val then "true" else "false");
  erlList = val: "[${concatStringsSep ", " (map toErl val)}]";
  erlTuple = val: "{${concatStringsSep ", " (map toErl val)}}";

  erlPropListTerms = val: let
    mkExTuple = name: extension: erlTuple ([{ atom = name; }] ++ extension);
    mkFreeKey = key: value: "{${toErl key}, ${toErl value}}";
    mkMulti = name: value:
      if isAttrs value && value ? ${name} then mkTuple name value.${name}
      else if isAttrs value then mkTuple name value
      else singleton "{${erlAtom name}, ${toErl value}}";
    mkTuple = name: value:
      if value ? flag then optional value.flag (erlAtom name)
      else if value ? multi then lib.concatMap (mkMulti name) value.multi
      else if value ? extuple then singleton (mkExTuple name value.extuple)
      else if value ? freekey && value ? value
           then singleton (mkFreeKey value.freekey value.value)
      else singleton "{${erlAtom name}, ${toErl value}}";
  in lib.concatLists (mapAttrsToList mkTuple val);

  erlPropList = val: "[${concatStringsSep ", " (erlPropListTerms val)}]";
  erlTermList = val: concatStringsSep ".\n" (erlPropListTerms val) + ".\n";

  toErl = val: let
    nonNix = attr:
      if attr ? __raw then attr.__raw
      else if attr ? atom then erlAtom attr.atom
      else if attr ? tuple then erlTuple attr.tuple
      else if attr ? binary then erlBinary attr.binary
      else erlPropList attr;
    nix = term:
      if builtins.isInt term then toString term
      else if builtins.isBool term then erlBool term
      else if builtins.isString term then erlString term
      else if isList term then erlList term
      else throw ("Can't transform value (${lib.showVal term}) into an " +
                  "Erlang expression!");
  in (if isAttrs val then nonNix else nix) val;

  types.erlPropList = mkOptionType {
    name = "an Erlang property list";
    check = lib.types.attrs.check;
    merge = loc: defs: erlPropList (lib.types.attrs.merge loc defs);
  };

  erlType = valType: mkOptionType {
    name = "an Erlang expression";
    merge = loc: defs: valType (mergeOneOption loc defs);
  };

  parseErlIpAddr = addr: let
    throwInvalid = throw "Invalid IPv4 or IPv6 address: ${addr}";
    tuplize = val: "{${concatStringsSep ", " val}}";

    v4DigitRe = "(25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9][0-9]|[0-9])";
    v6DigitRe = "([0-9a-fA-F]{0,4})";
    v4Re = "${v4DigitRe}\\.${v4DigitRe}\\.${v4DigitRe}\\.${v4DigitRe}";
    v4Parse = builtins.match v4Re addr;

    v6Expanded = let
      splitted = let
        sanityCheck = builtins.match ".*(PAD|:::).*" addr != null;
        splitter = val: let
          result = builtins.match "([^:]*):(.*)" val;
          iter = [ (head result) ] ++ splitter (last result);
        in if result == null then [ val ] else iter;
        prepared = splitter (replaceStrings ["::"] [":PAD:"] addr);
        tooManyPads = length (filter (x: x == "PAD") prepared) > 1;
        simple = if tooManyPads || sanityCheck then throwInvalid else prepared;
        v4mapped = builtins.match v4Re (last simple);
        rewritten = init simple ++ [ (take 2 v4mapped) (drop 2 v4mapped) ];
      in if v4mapped != null then rewritten else simple;
      folder = acc: digit: let
        afterAcc = length splitted - length acc;
        accLen = length acc;
        invalid = padLen < 1 && afterAcc > 2 && accLen > 1;
        padLen = 8 - accLen - afterAcc + 1;
        pad = genList (const "0") (if invalid then throwInvalid else padLen);
      in if digit == "PAD" then acc ++ pad else acc ++ [ digit ];
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
        converted = toString (d1 * 4096 + d2 * 256 + d3 * 16 + d4);
      in if digit == "" then "0" else converted;

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
