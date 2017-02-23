{ options, config, lib, ... }:

let
  matchType = lib.mkOptionType {
    name = "string, `{ regex = string; }' or `{ glob = string; }'";
    check = m: let
      isOnly = attr: lib.attrNames m == [ attr ];
    in lib.isString m || (lib.isAttrs m && (isOnly "regex" || isOnly "glob"));
    merge = lib.mergeOneOption;
  };

  mkAclOption = what: lib.mkOption {
    type = matchType;
    example.regex = "^[0-9]+$";
    description = ''
      Match a specific ${what}.

      <itemizedlist>
        <listitem><para>If the value is a plain string, an exact match is done
        against the ${what}.</para></listitem>

        <listitem><para>If the value is an attribute set like
        <literal>{ regex = "SOME_REGEX"; }</literal> the ${what} is matched
        against a regular expression. The syntax of this regular expression is
        PCRE with a few Erlang-specific differences, see <link
        xlink:href="http://erlang.org/doc/man/re.html#regexp_syntax_details"/>.
        </para></listitem>

        <listitem><para>If the value is an attribute set like
        <literal>{ glob = "SOME_REGEX"; }</literal> the ${what} is matched
        using shell glob wildcards like described in <citerefentry>
          <refentrytitle>glob</refentrytitle>
          <manvolnum>7</manvolnum>
        </citerefentry>.</para></listitem>
      </itemizedlist>
    '';
  };

  optAttrs = [ "user" "server" "resource" ];
  genOptAttrs = lib.genAttrs optAttrs;

  genDefined = opt:
    if !options.${opt}.isDefined then null
    else if config.${opt} ? regex then "regex"
    else if config.${opt} ? glob then "glob"
    else "plain";

  definedOptions = lib.genAttrs optAttrs genDefined;

  transform = transfs: let
    current = lib.head transfs;
    pprintDef = { opt, val }:
      if val ? regex then "${opt}.regex = `${val.regex}'"
      else if val ? glob then "${opt}.glob = `${val.glob}'"
      else "${opt} = `${val}'";
    defs = lib.fold (opt: acc: let
      val = config.${opt};
      inherit (options.${opt}) isDefined;
    in acc ++ lib.optional isDefined { inherit opt val; }) [] optAttrs;
    what = lib.concatMapStringsSep " and " pprintDef defs;
    notFound = "Unable to find a valid MongooseIM ACL match rule for ${what}.";
  in if transfs == [] then throw notFound
     else if current.matches == definedOptions then current.result
     else transform (lib.tail transfs);

  mkRule = resultAtom: transfs: let
    lookupTrans = trans:
      if lib.isList trans then lib.getAttrFromPath trans config
      else config.${trans};

  in {
    matches = let
      nullAttrs = lib.genAttrs optAttrs (lib.const null);
      transAttrs = map (path: {
        name = if lib.isList path then lib.head path else path;
        value = if lib.isList path then lib.last path else "plain";
      }) transfs;
    in nullAttrs // lib.listToAttrs transAttrs;

    result = {
      tuple = lib.singleton {
        atom = resultAtom;
      } ++ map lookupTrans transfs;
    };
  };

  transformed = transform [
    (mkRule "user" [ "user" ])
    (mkRule "user" [ "user" "server" ])
    (mkRule "server" [ "server" ])
    (mkRule "resource" [ "resource" ])
    (mkRule "user_regexp" [ ["user" "regex"] ])
    (mkRule "user_regexp" [ ["user" "regex"] "server" ])
    (mkRule "server_regexp" [ ["server" "regex"] ])
    (mkRule "resource_regexp" [ ["resource" "regex"] ])
    (mkRule "node_regexp" [ ["user" "regex"] ["server" "regex"] ])
    (mkRule "user_glob" [ ["user" "glob"] ])
    (mkRule "user_glob" [ ["user" "glob"] "server" ])
    (mkRule "server_glob" [ ["server" "glob"] ])
    (mkRule "resource_glob" [ ["resource" "glob"] ])
    (mkRule "node_glob" [ ["user" "glob"] ["server" "glob"] ])
  ];

in {
  options = {
    user = mkAclOption "user name";
    server = mkAclOption "host name";
    resource = mkAclOption "resource";
    all = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Match on everything if <literal>true</literal> and overrides all of the
        other matches.
      '';
    };

    match = lib.mkOption {
      type = lib.types.attrs;
      internal = true;
      description = ''
        The resulting Nix expression that's translated to an Erlang expression
        for the options given by one of the match options.
      '';
    };
  };

  config.match = if config.all then { atom = "all"; } else transformed;
}
