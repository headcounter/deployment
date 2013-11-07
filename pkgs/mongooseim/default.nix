{ stdenv, buildErlang, fetchurl, pam, zlib, openssl, expat
, cuesport, redo, exml, lager, cowboy, folsom, mochijson2, alarms
}:

buildErlang rec {
  name = "esl-ejabberd";
  version = "1.2.2";

  src = fetchurl {
    url = "https://elearning.erlang-solutions.com/binaries/"
        + "sources/${name}-${version}.tar.gz";
    sha256 = "1w3v5fhz9f717cxx9ywy418zfs73sgcwv9cpgnpzmbp4xbxmjp6m";
  };

  patches = let
    ghPatch = { rev, sha256 }: fetchurl {
      url = "https://github.com/esl/ejabberd/commit/${rev}.patch";
      inherit sha256;
    };
  in (map ghPatch [
    { rev = "11bddb62c20b22e3991d5afcbaec3e5fd28c1828";
      sha256 = "0d3i3x5x3k0cq8676vfk98p0sllhzbxxhpifak2sp8djmms3qlm2";
    }
    { rev = "e6508cab7e94a3d6359c30481f86c234f2fb87e0";
      sha256 = "1ay02qh3xpqhv5g4fny7n95h8gvd4kbbp4m449r0zzd3yzb0lkzf";
    }
    { rev = "0385349f41791f558e802adf809d4fbf65ae360f";
      sha256 = "0knza0mv62gkqnjyb1lnl4l9rwdjdbcqv14xwmr3c2nq6vzfihrm";
    }
  ]) ++ [ ./reltool.patch ./journald.patch ./systemd.patch ];

  buildInputs = [ pam zlib openssl expat ];
  erlangDeps = [ cuesport redo exml lager cowboy folsom mochijson2 alarms ];

  postBuild = ''
    rebar generate
  '';

  postInstall = ''
    cp -a rel/ejabberd "$out"
  '';

  meta = {
    homepage = "https://www.erlang-solutions.com/products/"
             + "mongooseim-massively-scalable-ejabberd-platform";
    description = "An Ejabberd fork utilizing Erlang/OTP features.";
    license = stdenv.lib.licenses.gpl2Plus;
  };
}
