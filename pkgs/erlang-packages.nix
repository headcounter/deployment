{ pkgs, buildErlang }:

let
  inherit (pkgs) fetchurl fetchgit fetchFromGitHub;
in rec {
  alarms = buildErlang {
    name = "alarms";
    version = "0.1";

    src = fetchgit {
      url = "https://github.com/chrzaszcz/alarms.git";
      rev = "dcd64216c5fffcdde80e779b161f195a03294a8a";
      sha256 = "05h0s3brydxnjzpc39d5fy8n1in3v8y87n8mzix3zcqr9fknh8xg";
    };

    erlangDeps = [ folsom ];
  };

  base16 = buildErlang rec {
    name = "base16";
    version = "0.1";

    src = fetchgit {
      url = "https://github.com/goj/base16.git";
      rev = "ec420aa4ce0fb971f155274c606e00188a2ed37d";
      sha256 = "0wlxqb8w6nimjy5w9njmhmn8nn7jhsgv3y6fz3m0zr0iabpgiigy";
    };
  };

  bear = buildErlang rec {
    name = "bear";
    version = "0.1.3";

    src = fetchurl {
      url = "https://github.com/boundary/${name}/archive/${version}.tar.gz";
      sha256 = "0za2rg6vchb5c30lbhwsz71ylrjr5ms20ns0vgs5jhlaj5j7bxbd";
    };
  };

  cowboy = buildErlang rec {
    name = "cowboy";
    version = "1.0.1";

    src = fetchFromGitHub {
      owner = "ninenines";
      repo = "cowboy";
      rev = version;
      sha256 = "020as7fjjgl48g75q82z31fhw7pdnwyp0an788vfivjf0v6knakm";
    };

    erlangDeps = [ ranch cowlib ];
  };

  cowlib = buildErlang rec {
    name = "cowlib";
    version = "1.0.1";

    src = fetchFromGitHub {
      repo = "cowlib";
      owner = "ninenines";
      rev = version;
      sha256 = "1vrv5dgrqvdvm45g7chwmbfjyx9hd7wdk5fmzdlmv7zxagz0albc";
    };
  };

  cucumberl = buildErlang rec {
    name = "cucumberl";
    version = "0.0.5";

    src = fetchgit {
      url = "https://github.com/madtrick/cucumberl.git";
      rev = "3f2cca66ed87a53a64177232428ffde606bdcb9a";
      sha256 = "0ycprism4hxsnq08hal5wv2fmjizcaxb4i2mz7a84gyh98qb2rkb";
    };

    postPatch = ''
      sed -i -e 's/git/"0.0.5"/' examples/*/src/*.app.src
    '';
  };

  cuesport = buildErlang {
    name = "cuesport";
    version = "0.1";
    src = fetchgit {
      url = "https://github.com/goj/cuesport.git";
      rev = "3b16d99d8bc41774dbc4dabad8054b4423dec5a6";
      sha256 = "0al9wch2ypwpw3wqbk0sk6bjb232nzh37n5v4niy4ws5a51hm78c";
    };
  };

  ecoveralls = buildErlang rec {
    name = "ecoveralls";
    version = "0.1";

    src = fetchFromGitHub {
      repo = "ecoveralls";
      owner = "nifoc";
      rev = "0e52c4709f763d512e6972e91330977cfedb3d13";
      sha256 = "0p5apdzfncn60rkg7lvn2dvkqh0jcqiq7ba177lvccw7grvmnd0s";
    };

    erlangDeps = [ jsx ];
  };

  edown = buildErlang rec {
    name = "edown";
    version = "0.4";

    src = fetchFromGitHub {
      repo = "edown";
      owner = "esl";
      rev = version;
      sha256 = "0sq7hc7mmcv8maxg2map9czr2772gsbrjsr1gffk7r5f12lc7ffv";
    };
  };

  escalus = buildErlang rec {
    name = "escalus";
    version = "2.6.0";

    src = fetchFromGitHub {
      owner = "esl";
      repo = "escalus";
      rev = version;
      sha256 = "1p5bdjhdaxhl47z8b8awv0wa6hsm4590d5x61zib2nnphhpblnn7";
    };

    erlangDeps = [ exml fusco base16 lhttpc wsecli ];
  };

  espec = buildErlang rec {
    name = "espec";
    version = "1";

    src = fetchgit {
      url = "https://github.com/lucaspiller/${name}.git";
      rev = "44dd72b8924425f09ad1093226ce0c755524a507";
      sha256 = "1jd7kkmmzgrb5x09ga7vmnq600jsc9m25i9x97wlnvjdk7fghjfx";
    };

    erlangDeps = [ reloader ];
  };

  exml = buildErlang rec {
    name = "exml";
    version = "2.1.5";

    src = fetchFromGitHub {
      owner = "esl";
      repo = "exml";
      rev = version;
      sha256 = "1qmixn7i4gvc080pvhy9c0pwlswyshnghwavg4y36x1sl8rhcv7g";
    };

    buildInputs = [ pkgs.expat ];
  };

  folsom = buildErlang rec {
    name = "folsom";
    version = "0.7.4";

    src = fetchurl {
      url = "https://github.com/boundary/${name}/archive/${version}.tar.gz";
      sha256 = "0dg62i92lfc2k2p4zn78w61iiw09fm0pamx11ljb6c0rpkqwx8jv";
    };

    erlangDeps = [ bear meck ];
  };

  fusco = buildErlang rec {
    name = "fusco";
    version = "0.0.0";

    src = fetchFromGitHub {
      owner = "esl";
      repo = "fusco";
      rev = "78650a15cf244065ab3ee74dafb8efabfd73575d";
      sha256 = "05idlhxwlk5l0xni9fc52ncp3isin7k0gdzxzgifw1mk1157cg8g";
    };
  };

  goldrush = buildErlang rec {
    name = "goldrush";
    version = "0.1.6";

    src = fetchFromGitHub {
      owner = "DeadZen";
      repo = "goldrush";
      rev = version;
      sha256 = "0fhi3jidn40gri49scvqvavqxh0ggfllx4xii8yqrs0l2l4lq9b5";
    };
  };

  hamcrest = buildErlang rec {
    name = "hamcrest";
    version = "0.1.0";

    src = fetchgit {
      url = "https://github.com/hyperthunk/hamcrest-erlang.git";
      rev = "7215234e14a7c82458829c542edbf2899ceedbd3";
      sha256 = "0v6fg8bbvv1cx5qjff6l880r1b9fgrr8czrxw2bignw5mbrfdlx9";
    };
  };

  jsx = buildErlang rec {
    name = "jsx";
    version = "2.4.0";

    src = fetchFromGitHub {
      repo = "jsx";
      owner = "talentdeficit";
      rev = "v${version}";
      sha256 = "0kh1084ivva8wsl1545b0idd9g2r6q1ldf5cpgk2a36wmj2ni7zz";
    };
  };

  katt = buildErlang rec {
    name = "katt";
    version = "1.3.0-rc";

    src = fetchFromGitHub {
      owner = "for-GET";
      repo = "katt";
      rev = version;
      sha256 = "0m7r99wbbcdbi666zsp40lmy39an5lhc3rgr1hcac161701dk6vw";
    };

    postPatch = ''
      patchShebangs priv/compile-parser
    '';

    erlangDeps = [ mochijson3 lhttpc neotoma meck ];
  };

  lager = buildErlang rec {
    name = "lager";
    version = "2.1.0";

    src = fetchFromGitHub {
      repo = "lager";
      owner = "basho";
      rev = version;
      sha256 = "1bwd4g8bzh9msxzpp5nm4sssh883xycfrp76nk63r3vdah2ypmsv";
    };

    erlangDeps = [ goldrush ];
  };

  lhttpc = buildErlang rec {
    name = "lhttpc";
    version = "1.2.6";

    src = fetchgit {
      url = "https://github.com/esl/lhttpc.git";
      rev = "3c7fdeee241b6813efddcb08ad1697186780d385";
      sha256 = "01fxdrcpgsi7iq7pjagzvkp5vspsp9fbvjwmqfhnb9mksf3pxryc";
    };
  };

  meck = buildErlang rec {
    name = "meck";
    version = "0.8.1";

    src = fetchurl {
      url = "https://github.com/eproxus/${name}/archive/${version}.tar.gz";
      sha256 = "12h4bxcj0ka6zl7zngnbhly14yfv7aslm30xwx9l9sxrlvcxsfj4";
    };
  };

  mochijson2 = buildErlang {
    name = "mochijson2";
    version = "0.1";
    src = fetchgit {
      url = "https://github.com/bjnortier/mochijson2.git";
      rev = "3663fb01fd98958181adc2d1300c7bfa553e1434";
      sha256 = "1ss9hyjbr1yk3khca30qh6xlrzcdm6jrmxyyx99vhbb7cg8l6k35";
    };
  };

  mochijson3 = buildErlang {
    name = "mochijson3";
    version = "1.0";

    src = fetchFromGitHub {
      repo = "mochijson3";
      owner = "tophitpoker";
      rev = "1a1c913ac80bb45d3de5fbd74d21e96c45e9e844";
      sha256 = "18dvmxzsqmjav30d7l6x3dslmvc4x5skhvmscv5ppymj0y0pw3hz";
    };
  };

  mustache = buildErlang rec {
    name = "mustache";
    version = "0.1.0";

    src = fetchgit {
      url = "https://github.com/mojombo/mustache.erl.git";
      rev = "c0154ce140d7c5b088eee6aaa05226b388583ef4";
      sha256 = "0hasbajn7sdz5x3j3q66qbyd1ffgdg5gm8iw1kkkj8811d851afq";
    };

    erlangDeps = [ meck ];
  };

  neotoma = buildErlang rec {
    name = "neotoma";
    version = "1.7.2";

    src = fetchFromGitHub {
      owner = "seancribbs";
      repo = "neotoma";
      rev = version;
      sha256 = "0h4q5k8fb4l9iqkqsza8jpzg7pzprjx16pkfyz5fxbh02xig1r06";
    };
  };

  p1_cache_tab = buildErlang {
    name = "p1_cache_tab";
    version = "0.1.0";

    src = fetchFromGitHub {
      owner = "processone";
      repo = "cache_tab";
      rev = "7b89d6afb66d8ff9d56671864be74654f5b18e2f";
      sha256 = "1hw3hgzddcanzs6w88n66j2kdyz44zjayjwc3pg88bcr4rcwx46f";
    };

    erlangDeps = [ p1_utils ];
  };

  p1_stringprep = buildErlang {
    name = "p1_stringprep";
    version = "0.1.0";

    src = fetchFromGitHub {
      owner = "processone";
      repo = "stringprep";
      rev = "9e9e0f8dbe6a70ef36e1d4436b458ca5a77fbcfb";
      sha256 = "0q6xkywanh2wjjr0601pqh63qm08bq1firap7n3sdcfh0h0d9vnx";
    };
  };

  p1_utils = buildErlang {
    name = "p1_utils";
    version = "1";

    src = fetchFromGitHub {
      owner = "processone";
      repo = "p1_utils";
      rev = "9e646e4ff343e8e902410fa1fe28803202b7e340";
      sha256 = "0rlxgw4gsxacihlriv5spdnva88vygpx659m6x8bvqqmd6yhnpgr";
    };
  };

  pa = buildErlang rec {
    name = "pa";
    version = "0.2.0";

    src = fetchFromGitHub {
      repo = "pa";
      owner = "lavrin";
      rev = version;
      sha256 = "1kzh2g71sim98jd03xh697s8q0az0ma2p2inqc8cwhhr1lyfj2yp";
    };

    erlangDeps = [ proper ];
  };

  proper = buildErlang rec {
    name = "proper";
    version = "1.1";

    src = fetchFromGitHub {
      owner = "manopapad";
      repo = "proper";
      rev = "v${version}";
      sha256 = "14g5sjspg0vc3jfr1m8pq6fsj3wlj72pad1qfw1w4vx87z15xzq6";
    };

    postPatch = ''
      patchShebangs write_compile_flags
    '';
  };

  ranch = buildErlang rec {
    name = "ranch";
    version = "1.1.0";

    src = fetchFromGitHub {
      repo = "ranch";
      owner = "ninenines";
      rev = version;
      sha256 = "02b6nzdllrym90a5bhzlz4s52hyj9vwcn048na4j5jiivknm8g3r";
    };
  };

  rebar_feature_runner = buildErlang rec {
    name = "rebar_feature_runner";
    version = "0.1";

    src = fetchgit {
      url = "https://github.com/madtrick/rebar_feature_runner.git";
      rev = "bcbf1ba233a5f8388f6d530c707d98db2021a48a";
      sha256 = "1qi9lkkj0w86w2adn7mj119p6dn6yjvm61zhzmb5qj330gg2k1s9";
    };
  };

  redo = buildErlang {
    name = "redo";
    version = "1.1.0";
    src = fetchgit {
      url = "https://github.com/JacobVorreuter/redo.git";
      rev = "7c7eaef4cd65271e2fc4ea88587e848407cf0762";
      sha256 = "1i1lgxw5iasf49cvsyfcrv0sv1yxlzf06yi40w8ygd8yid6z1siv";
    };
  };

  reloader = buildErlang rec {
    name = "reloader";
    version = "1";

    src = fetchgit {
      url = "https://github.com/lucaspiller/${name}.git";
      rev = "9dd05d613c2abe563bc1c472950b96d2a832663b";
      sha256 = "0jdg3sp005n6jjh7di4xcikmjzzd80lnkr64k3x3a3xzd5vmmqxh";
    };
  };

  seestar = buildErlang {
    name = "seestar";
    version = "0.0.1";

    src = fetchFromGitHub {
      repo = "seestar";
      owner = "iamaleksey";
      rev = "94b17823f182fef20f878b19ea20761c00795879";
      sha256 = "13l47hj7lm9ciqyk0rk3pzllj12141jcqkr5j7hpnwg44j4xd8wm";
    };

    erlangDeps = [ edown ];
  };

  wsecli = buildErlang rec {
    name = "wsecli";
    version = "1";

    src = fetchgit {
      url = "https://github.com/madtrick/wsecli.git";
      rev = "752d062af4fc943414505ce846d5317d003d2dbc";
      sha256 = "0i3bzf7chn6aj7xn8lzf3kh6gy5mr7d6738i0lhfx21y4pyw1xri";
    };

    erlangDeps = [ espec cucumberl hamcrest meck rebar_feature_runner wsock ];
  };

  wsock = buildErlang rec {
    name = "wsock";
    version = "1.0.2";

    src = fetchurl {
      url = "https://github.com/madtrick/wsock/archive/${version}.tar.gz";
      sha256 = "1sch3g2x01a346bmasgz09k2m66nridkdz7xqi2i846ng4440njs";
    };
  };
}
