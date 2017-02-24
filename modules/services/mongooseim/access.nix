{ type, optPrefix, patterns }:

{ config, lib, ... }:

let
  checkPattern = passthru: let
    pattern = config.match;
    isValid = pattern == null || patterns ? ${pattern};
    patOpt = "${optPrefix}.acl.patterns";
    err = "No pattern `${pattern}' defined in `${patOpt}'.";
  in if isValid then passthru else throw err;

  mkResult = resType: {
    tuple = [
      resType
      { atom = if config.match == null then "all" else config.match; }
    ];
  };

  optsAndResults = if type == "shaper" then {
    options.shaper = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      description = ''
        A shaper name defined by <option>${optPrefix}.shapers</option>, where
        its rate value is returned on a successful <option>match</option>.

        If the value is <literal>null</literal>, no shaping is done for the
        operation.
      '';
    };
    result = mkResult {
      atom = if config.shaper == null then "none" else config.shaper;
    };
  } else if type == "limit" then {
    options.limit = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      description = ''
        A limit as an integer or <literal>null</literal> for no limit, where the
        limit is returned on a successful <option>match</option>.
      '';
    };
    result = mkResult (if config.limit == null then {
      atom = "infinity";
    } else config.limit);
  } else if type == "access" then {
    options.allow = lib.mkOption {
      type = lib.types.bool;
      description = ''
        Whether to allow (<literal>true</literal>) or deny
        (<literal>false</literal>) an operation on a successful
        <option>match</option>.
      '';
    };
    result = mkResult {
      atom = if config.allow then "allow" else "deny";
    };
  } else throw "Invalid access submodule type ${type}.";

in {
  options = {
    match = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Match an ACL pattern defined by
        <option>${optPrefix}.acl.patterns</option> or <literal>null</literal>
        to match on <emphasis>all</emphasis> JIDs.
      '';
    };

    expression = lib.mkOption {
      type = lib.types.attrs;
      internal = true;
      description = ''
        The resulting Nix expression that's translated to an Erlang expression
        for the options given by this submodule.
      '';
    };
  } // optsAndResults.options;

  config.expression = checkPattern optsAndResults.result;
}
