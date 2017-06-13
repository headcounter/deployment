{ stdenv, buildErlang, fetchFromGitHub, pam, zlib, expat, writeText
, erlangPackages

, storageBackends ? [ "pgsql" ]
}:

let
  inherit (stdenv.lib) concatLists concatMap concatMapStringsSep;

  backendDeps = {
    pgsql = [ "p1_pgsql" ];
    mysql = [ "p1_mysql" ];
    riak = [ "riakc" ];
    cassandra = [ "cqerl" ];
  };

  getErlDeps = backend: backendDeps.${backend} or [];
  enabledBackendDeps = concatMap getErlDeps storageBackends;
  backendErlangDeps = map (dep: erlangPackages.${dep}) enabledBackendDeps;

  internalVersion = "2.1.8+mim-${self.version}";

  relativeMainAppDir = let
    hash = builtins.head (builtins.match "([^-]*).*" (baseNameOf self));
    hashVer = "${hash}_${internalVersion}";
  in "lib/ejabberd-${hashVer}";

  self = buildErlang rec {
    name = "mongooseim";
    version = "2.0.1";

    src = fetchFromGitHub {
      owner = "esl";
      repo = "MongooseIM";
      rev = version;
      sha256 = "18j2chhvq1pm78ay3gb19yjqifdyzxgbjz8hycjp529sxji3fdzq";
    };

    patches = [
      ./reltool.patch ./journald.patch ./systemd.patch ./ctl-set-config.patch
      ./s2s-listener-certfile.patch ./pgsql-disable-escape-string-warning.patch
      ./tls-fixes.patch ./allow-set-default-mam-archive-mode.patch
    ];

    prePatch = ''
      sed -i -e 's/{vsn, {cmd, [^}]*}}/{vsn, "2.1.8+mim-${version}"}/' \
        apps/ejabberd/src/ejabberd.app.src
    '';

    postPatch = let
      inherit (stdenv.lib) subtractLists filterAttrs attrValues escapeShellArg;
      allBackends = [ "pgsql" "mysql" "riak" "cassandra" ];
      removedBackends = subtractLists storageBackends allBackends;
      mkFindRemoveArgs = concatMapStringsSep " -o " (rem: "-iname '*${rem}*'");
      filterBackend = backend: stdenv.lib.elem backend removedBackends;
      maybeRemoveSed = backend: let
        sedArg = "-e '/mongoose_${backend}:start/d'";
      in stdenv.lib.optionalString (filterBackend backend) sedArg;
      removedBackendDepAttrs = filterAttrs (n: v: filterBackend n) backendDeps;
      removedBackendDeps = concatLists (attrValues removedBackendDepAttrs);
      removedRebarDeps = [ "lager_syslog" ] ++ removedBackendDeps;
    in ''
      # Remove source files that have backends that should be removed in its
      # name.
      find apps \( ${mkFindRemoveArgs removedBackends} \) -type f -delete

      # Remove a bunch of rebar dependencies we don't need.
			escript "${writeText "remove-unneded-rebar-deps.escript" ''
        # Dummy comment, skipped by escript!
        main(Args) ->
          {ok, Orig} = file:consult("rebar.config"),
          {deps, OrigDeps} = lists:keyfind(deps, 1, Orig),
          DepsToRemove = lists:map(fun erlang:list_to_atom/1, Args),
          NewDeps = [Dep || {Name, _, _} = Dep <- OrigDeps,
                     not lists:member(Name, DepsToRemove)],
          ReplacedDeps = lists:keyreplace(deps, 1, Orig, {deps, NewDeps}),
          New = case lists:member(riakc, DepsToRemove) of
            true -> lists:keydelete(pre_hooks, 1, ReplacedDeps);
            false -> ReplacedDeps
          end,
          Data = lists:map(fun(T) -> io_lib:format("~tp.~n", [T]) end, New),
          file:write_file("rebar.config", Data).
      ''}" ${concatMapStringsSep " " escapeShellArg removedRebarDeps}

      # Remove occurences of mongoose_$backend:start() during app startup.
      sed -i ${concatMapStringsSep " " maybeRemoveSed ["riak" "cassandra"]} \
        apps/ejabberd/src/ejabberd_app.erl

      # Remove lager_syslog stuff, because we're doing logging via journald.
      sed -i -e '/^AppsToRun *= *\[/,/\]/{ /^ *\(lager_\)\?syslog *, *$/d }' \
        rel/reltool.config.script

      patchShebangs tools/configure
    '';

    postConfigure = let
      anyArgs = concatMapStringsSep " " (bend: "with-${bend}") storageBackends;
      backendArgs = if storageBackends != [] then anyArgs else "with-none";
    in "./tools/configure ${backendArgs}";

    buildInputs = [ pam zlib expat ];
    erlangDeps = with erlangPackages; [
      alarms base16 cache_tab cowboy cuesport ecoveralls exml exometer fast_tls
      folsom fusco idna jiffy lager lasse mochijson2 mustache pa proper poolboy
      recon redo sd_notify stringprep usec uuid
    ] ++ backendErlangDeps;

    postBuild = ''
      make rel/vars.config
      rebar generate
    '';

    installPhase = ''
      cp -a "rel/${name}" "$out"

      intVer=${stdenv.lib.escapeShellArg internalVersion}
      hashVer="$(basename "$out" | cut -d- -f1)_$intVer"
      mainAppDir="$out/lib/ejabberd-$hashVer"
      if [ ! -d "$mainAppDir" ]; then
        echo "$mainAppDir does not exist!" >&2
        exit 1
      fi
    '';

    passthru.mainAppDir = "${self}/${relativeMainAppDir}";

    meta = {
      homepage = "https://www.erlang-solutions.com/products/"
               + "mongooseim-massively-scalable-ejabberd-platform";
      description = "An Ejabberd fork utilizing Erlang/OTP features.";
      license = stdenv.lib.licenses.gpl2Plus;
    };
  };

in self
