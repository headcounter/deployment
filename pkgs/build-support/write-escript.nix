{ stdenv, erlang, writeText }:

name: emuArgs: textOrFile:

with stdenv.lib;
with import ../../modules/services/mongooseim/erlexpr.nix {
  inherit erlang;
  inherit (stdenv) lib;
};

let
  mkFile = writeText "${name}.erl";
  scriptFile = if isString textOrFile then mkFile textOrFile else textOrFile;

  erlCommands = writeText "write-escript.erl" ''
    % skipped line, escript is executed directly
    main([Name]) ->
      CompileOpts = [binary, {warn_format, 2}, report],
      {ok, _, BeamCode} = compile:file(Name, CompileOpts),
      EmuArgs = [${concatStringsSep ", " (map erlString emuArgs)}],
      MaybeEmuArgs = case length(EmuArgs) of
        0 -> [];
        _ -> [{emu_args, EmuArgs}]
      end,
      Out = case os:getenv("out") of
        false -> halt("Environment variable $out is not set!");
        Path  -> Path
      end,
      ok = escript:create(Out, [
        {shebang, "${erlang}/bin/escript"},
        {beam, BeamCode}
      ] ++ MaybeEmuArgs),
      halt(0).
  '';

in stdenv.mkDerivation {
  inherit name;
  buildCommand = ''
    cat "${scriptFile}" > "$name.erl"
    "${erlang}/bin/escript" "${erlCommands}" "$name.erl"
    chmod +x "$out"
  '';
}
