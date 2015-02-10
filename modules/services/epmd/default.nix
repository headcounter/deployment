{ pkgs, lib, config, ... }:

with lib;

let
  cfg = config.services.headcounter.epmd;

  epmdPatched = lib.overrideDerivation pkgs.erlang (o: {
    name = "epmd-${o.version}";

    patchPhase = null;
    postPatch = o.patchPhase;

    patches = singleton (pkgs.fetchpatch {
      url = "https://github.com/erlang/otp/compare/4e96974...5db2345.diff";
      sha256 = "0vz1zl4qb0rwyik87vlijp3k993kvnp5irjk8bxdj3d00s05i7aw";
    });

    preConfigure = o.preConfigure + ''
      export ERL_TOP="$(pwd)"
      cd erts
      autoreconf -vfi
    '';

    NIX_CFLAGS_COMPILE = "-I${pkgs.systemd}/include";
    NIX_LDFLAGS = "-L${pkgs.systemd}/lib";

    configureFlags = [
      "--disable-option-checking"
      "--enable-systemd"
    ];

    buildInputs = o.buildInputs ++ [ pkgs.autoconf pkgs.automake pkgs.libtool ];
    buildFlags = [ "-C epmd/src" ];

    installPhase = ''
      install -vD "$ERL_TOP"/bin/*/epmd "$out/bin/epmd"
    '';

    postFixup = null;
  });

  sockets = listToAttrs (map (addr: {
    name = "epmd-${replaceChars ["."] ["-"] addr}";
    value = {
      description = "Socket for EPMD address ${addr}";
      wantedBy = [ "sockets.target" ];
      socketConfig.ListenStream = "${addr}:${toString cfg.port}";
      socketConfig.Service = "epmd.service";
    };
  }) cfg.addresses);

in {
  options.services.headcounter.epmd = {
    enable = mkEnableOption "Erlang port mapper daemon";

    port = mkOption {
      type = types.int;
      default = 4369;
      description = ''
        The TCP port to listen for incoming name queries.
      '';
    };

    addresses = mkOption {
      type = types.listOf types.str;
      default = [ "127.0.0.1" ];
      description = ''
        Addresses to listen for incoming name queries.
      '';
    };
  };

  config = mkIf cfg.enable {
    users.extraGroups.epmd = {};
    users.extraUsers.epmd = {
      description = "Erland Port Mapper User";
      group = "epmd";
    };

    systemd.sockets = sockets;

    systemd.services.epmd = {
      description = "Erlang Port Mapper Daemon";
      serviceConfig.ExecStart = toString [
        "@${epmdPatched}/bin/epmd" "epmd" "-debug" "-systemd"
      ];
      serviceConfig.Sockets = map (s: "${s}.socket") (attrNames sockets);
      serviceConfig.User = "epmd";
      serviceConfig.Group = "epmd";
    };
  };
}
