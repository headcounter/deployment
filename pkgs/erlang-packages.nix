{ pkgs, buildErlang }:

let
  inherit (pkgs) fetchFromGitHub;

  ghe = name: version: owner: sha256: overrides: buildErlang ({
    inherit name version;
    src = fetchFromGitHub ({
      rev = version;
      repo = name;
      inherit owner sha256;
    } // (overrides.src or {}));
  } // builtins.removeAttrs overrides ["src"]);

  shas = {
    alarms        = "1xdk05iq691qxjzb25znyc4s9q3j006jgxkqvnwagd46yz6bnlx0";
    base16        = "0kq6x40543sc2bkphj5pf83m9sc6knf5j83nihpp2x7wp6n704sk";
    bear          = "1x80qwyx56xclqhmcpdg082w1pbsw8jc9fa79hqy6q5i419w2wrg";
    cowboy        = "020as7fjjgl48g75q82z31fhw7pdnwyp0an788vfivjf0v6knakm";
    cowlib        = "1vrv5dgrqvdvm45g7chwmbfjyx9hd7wdk5fmzdlmv7zxagz0albc";
    cucumberl     = "0jby37zh7jzwv39fx2vh4cbi89syfilxdfx3qy7g9vjvmygzadrf";
    cuesport      = "0r89p9g5ps7rbd06rzzpyr7d3vm18bl9wpxy3sj2f551km248jbq";
    ecoveralls    = "0p5apdzfncn60rkg7lvn2dvkqh0jcqiq7ba177lvccw7grvmnd0s";
    edown         = "0sq7hc7mmcv8maxg2map9czr2772gsbrjsr1gffk7r5f12lc7ffv";
    escalus       = "1p5bdjhdaxhl47z8b8awv0wa6hsm4590d5x61zib2nnphhpblnn7";
    espec         = "1k070c54f6kcdk3ciipq9y651cmdci7g67kqmb4r1gib2y1apzad";
    exml          = "1qmixn7i4gvc080pvhy9c0pwlswyshnghwavg4y36x1sl8rhcv7g";
    folsom        = "0xfi5r4z0wq7sjpcyhc2w19jqj8g8qr9ifdrv77p5gj84rx1fxbq";
    fusco         = "05idlhxwlk5l0xni9fc52ncp3isin7k0gdzxzgifw1mk1157cg8g";
    goldrush      = "0fhi3jidn40gri49scvqvavqxh0ggfllx4xii8yqrs0l2l4lq9b5";
    hamcrest      = "1js9xmapavh50glfi062rlbg8nglq37gam6ahr9hq679bhsva3ka";
    jsx           = "0kh1084ivva8wsl1545b0idd9g2r6q1ldf5cpgk2a36wmj2ni7zz";
    katt          = "0m7r99wbbcdbi666zsp40lmy39an5lhc3rgr1hcac161701dk6vw";
    lager         = "1bwd4g8bzh9msxzpp5nm4sssh883xycfrp76nk63r3vdah2ypmsv";
    lhttpc        = "0gzi5b99cd1zi31429j3zhq5k2c7f2175afaay7ai234gvj21g7d";
    meck          = "07v788g6z2q04r879g71fyzpzwvm76ygnwvsp415nqv644rdv6nf";
    mochijson2    = "1ss9hyjbr1yk3khca30qh6xlrzcdm6jrmxyyx99vhbb7cg8l6k35";
    mochijson3    = "18dvmxzsqmjav30d7l6x3dslmvc4x5skhvmscv5ppymj0y0pw3hz";
    mustache      = "0nzna9jh5a0a6g738v04kq2vqxqvj5p7ngkngxqzrbmysrp8sz89";
    neotoma       = "0h4q5k8fb4l9iqkqsza8jpzg7pzprjx16pkfyz5fxbh02xig1r06";
    p1_cache_tab  = "1hw3hgzddcanzs6w88n66j2kdyz44zjayjwc3pg88bcr4rcwx46f";
    p1_stringprep = "0q6xkywanh2wjjr0601pqh63qm08bq1firap7n3sdcfh0h0d9vnx";
    p1_utils      = "0rlxgw4gsxacihlriv5spdnva88vygpx659m6x8bvqqmd6yhnpgr";
    pa            = "1kzh2g71sim98jd03xh697s8q0az0ma2p2inqc8cwhhr1lyfj2yp";
    proper        = "14g5sjspg0vc3jfr1m8pq6fsj3wlj72pad1qfw1w4vx87z15xzq6";
    ranch         = "02b6nzdllrym90a5bhzlz4s52hyj9vwcn048na4j5jiivknm8g3r";
    rebarFR       = "17rkqx1cx8nvg9f0zy97d7xpy877bm68y37ywbmv5irj6kwbsqkk";
    redo          = "14hg2jcs3qyl7aaz8ni9h8s97kjs0ksdfnh25m3hava7ga45jq1c";
    reloader      = "1ansv02klh9i53gmvxjk7vl83nsvyada58xn1pmc9gid0cl5vnl4";
    seestar       = "13l47hj7lm9ciqyk0rk3pzllj12141jcqkr5j7hpnwg44j4xd8wm";
    wsecli        = "1zddpvrm7lqx51d34plaql5ka8m9nazc7ycpmgb4b0jsdc2q39vx";
    wsock         = "1k5qwbh82jawwpa0j7x04nc379j9pzdsa42nj7j2ahpla786np0v";
  };
in rec {
  alarms = ghe "alarms" "0.1" "chrzaszcz" shas.alarms {
    src.rev = "dcd64216c5fffcdde80e779b161f195a03294a8a";
    erlangDeps = [ folsom ];
  };

  base16 = ghe "base16" "0.1" "goj" shas.base16 {
    src.rev = "ec420aa4ce0fb971f155274c606e00188a2ed37d";
  };

  bear = ghe "bear" "0.1.3" "boundary" shas.bear {};

  cowboy = ghe "cowboy" "1.0.1" "ninenines" shas.cowboy {
    erlangDeps = [ ranch cowlib ];
  };

  cowlib = ghe "cowlib" "1.0.1" "ninenines" shas.cowlib {};

  cucumberl = ghe "cucumberl" "0.0.5" "madtrick" shas.cucumberl {
    src.rev = "3f2cca66ed87a53a64177232428ffde606bdcb9a";
    postPatch = ''
      sed -i -e 's/git/"0.0.5"/' examples/*/src/*.app.src
    '';
  };

  cuesport = ghe "cuesport" "0.1" "goj" shas.cuesport {
    src.rev = "3b16d99d8bc41774dbc4dabad8054b4423dec5a6";
  };

  ecoveralls = ghe "ecoveralls" "0.1" "nifoc" shas.ecoveralls {
    src.rev = "0e52c4709f763d512e6972e91330977cfedb3d13";
    erlangDeps = [ jsx ];
  };

  edown = ghe "edown" "0.4" "esl" shas.edown {};

  escalus = ghe "escalus" "2.6.0" "esl" shas.escalus {
    erlangDeps = [ exml fusco base16 lhttpc wsecli ];
  };

  espec = ghe "espec" "1" "lucaspiller" shas.espec {
    src.rev = "44dd72b8924425f09ad1093226ce0c755524a507";
    erlangDeps = [ reloader ];
  };

  exml = ghe "exml" "2.1.5" "esl" shas.exml {
    buildInputs = [ pkgs.expat ];
  };

  folsom = ghe "folsom" "0.7.4" "boundary" shas.folsom {
    erlangDeps = [ bear meck ];
  };

  fusco = ghe "fusco" "0.0.0" "esl" shas.fusco {
    src.rev = "78650a15cf244065ab3ee74dafb8efabfd73575d";
  };

  goldrush = ghe "goldrush" "0.1.6" "DeadZen" shas.goldrush {};

  hamcrest = ghe "hamcrest" "0.1.0" "hyperthunk" shas.hamcrest {
    src.repo = "hamcrest-erlang";
    src.rev = "7215234e14a7c82458829c542edbf2899ceedbd3";
  };

  jsx = ghe "jsx" "2.4.0" "talentdeficit" shas.jsx {
    src.rev = "v2.4.0"; # XXX
  };

  katt = ghe "katt" "1.3.0-rc" "for-GET" shas.katt {
    postPatch = ''
      patchShebangs priv/compile-parser
    '';
    erlangDeps = [ mochijson3 lhttpc neotoma meck ];
  };

  lager = ghe "lager" "2.1.0" "basho" shas.lager {
    erlangDeps = [ goldrush ];
  };

  lhttpc = ghe "lhttpc" "1.2.6" "esl" shas.lhttpc {
    src.rev = "3c7fdeee241b6813efddcb08ad1697186780d385";
  };

  meck = ghe "meck" "0.8.1" "eproxus" shas.meck {};

  mochijson2 = ghe "mochijson2" "0.1" "bjnortier" shas.mochijson2 {
    src.rev = "3663fb01fd98958181adc2d1300c7bfa553e1434";
  };

  mochijson3 = ghe "mochijson3" "1.0" "tophitpoker" shas.mochijson3 {
    src.rev = "1a1c913ac80bb45d3de5fbd74d21e96c45e9e844";
  };

  mustache = ghe "mustache" "0.1.0" "mojombo" shas.mustache {
    src.repo = "mustache.erl";
    src.rev = "c0154ce140d7c5b088eee6aaa05226b388583ef4";
    erlangDeps = [ meck ];
  };

  neotoma = ghe "neotoma" "1.7.2" "seancribbs" shas.neotoma {};

  p1_cache_tab = ghe "p1_cache_tab" "0.1.0" "processone" shas.p1_cache_tab {
    src.repo = "cache_tab";
    src.rev = "7b89d6afb66d8ff9d56671864be74654f5b18e2f";
    erlangDeps = [ p1_utils ];
  };

  p1_stringprep = ghe "p1_stringprep" "0.1.0" "processone" shas.p1_stringprep {
    src.repo = "stringprep";
    src.rev = "9e9e0f8dbe6a70ef36e1d4436b458ca5a77fbcfb";
  };

  p1_utils = ghe "p1_utils" "1" "processone" shas.p1_utils {
    src.rev = "9e646e4ff343e8e902410fa1fe28803202b7e340";
  };

  pa = ghe "pa" "0.2.0" "lavrin" shas.pa {
    erlangDeps = [ proper ];
  };

  proper = ghe "proper" "1.1" "manopapad" shas.proper {
    src.rev = "v1.1"; # XXX
    postPatch = ''
      patchShebangs write_compile_flags
    '';
  };

  ranch = ghe "ranch" "1.1.0" "ninenines" shas.ranch {};

  rebarFR = ghe "rebar_feature_runner" "0.1" "madtrick" shas.rebarFR {
    src.rev = "bcbf1ba233a5f8388f6d530c707d98db2021a48a";
  };

  redo = ghe "redo" "1.1.0" "JacobVorreuter" shas.redo {
    src.rev = "7c7eaef4cd65271e2fc4ea88587e848407cf0762";
  };

  reloader = ghe "reloader" "1" "lucaspiller" shas.reloader {
    src.rev = "9dd05d613c2abe563bc1c472950b96d2a832663b";
  };

  seestar = ghe "seestar" "0.0.1" "iamaleksey" shas.seestar {
    src.rev = "94b17823f182fef20f878b19ea20761c00795879";
    erlangDeps = [ edown ];
  };

  wsecli = ghe "wsecli" "1" "madtrick" shas.wsecli {
    src.rev = "752d062af4fc943414505ce846d5317d003d2dbc";
    erlangDeps = [ espec cucumberl hamcrest meck rebarFR wsock ];
  };

  wsock = ghe "wsock" "1.0.2" "madtrick" shas.wsock {};
}
