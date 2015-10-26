{ pkgs, buildErlang, writeEscript }:

let
  ghe = version: owner: name: with pkgs.lib; let
    fixOvr = overrides.${name} or {};

    erlAttrs = ovr: {
      inherit name version;
      src = {
        rev = revMap.${name} or version;
        repo = ovr.name or name;
        sha256 = getAttr name shaSums;
        inherit owner;
      } // (ovr.src or {});
    } // optionalAttrs ((depMap self) ? ${name}) {
      erlangDeps = (depMap self).${name};
    } // removeAttrs ovr ["src"];

    overrideFun = overrideFuns.${name} or id;

    stage1 = erlAttrs fixOvr;
    stage2 = erlAttrs (fixOvr // overrideFun stage1);

  in buildErlang (stage2 // {
    src = pkgs.fetchFromGitHub stage2.src;
  });

  overrides = {
    exml.buildInputs = [ pkgs.expat ];
    exometer.EXOMETER_PACKAGES = "(minimal)";
    hamcrest.src.repo = "hamcrest-erlang";
    idna.src.repo = "erlang-idna";
    katt.postPatch = ''
      cat "${writeEscript "compile-parser" [] ''
        -module('compile-parser').
        -export([main/1]).

        main(_) ->
          code:add_pathz("${self.neotoma}/lib/erlang/lib/neotoma/ebin"),
          neotoma:file("priv/katt_blueprint.peg", [
            {output, "src/"},
            {neotoma_priv_dir, "${self.neotoma}/lib/erlang/lib/neotoma/priv"}
          ]).
      ''}" > priv/compile-parser
    '';
    mustache.src.repo = "mustache.erl";
    p1_cache_tab.src.repo = "cache_tab";
    p1_stringprep.src.repo = "stringprep";
    proper.postPatch = ''
      patchShebangs write_compile_flags
    '';
    rebarFR.name = "rebar_feature_runner";
  };

  overrideFuns = {
    cucumberl = pkg: {
      postPatch = ''
        sed -i -e 's/git/"${pkg.version}"/' examples/*/src/*.app.src
      '';
    };
  } // pkgs.lib.genAttrs [ "jsx" ] (pkgs.lib.const (pkg: {
    src = (pkg.src or {}) // { rev = "v${pkg.version}"; };
  }));

  revMap = {
    alarms        = "0c40c6cdfcad6cb89313621150dcea5237bb8478";
    base16        = "ec420aa4ce0fb971f155274c606e00188a2ed37d";
    cucumberl     = "3f2cca66ed87a53a64177232428ffde606bdcb9a";
    cuesport      = "3b16d99d8bc41774dbc4dabad8054b4423dec5a6";
    ecoveralls    = "0e52c4709f763d512e6972e91330977cfedb3d13";
    escalus       = "51a82dc9393233855ae2923104e46bfaf74d8943";
    espec         = "44dd72b8924425f09ad1093226ce0c755524a507";
    fusco         = "78650a15cf244065ab3ee74dafb8efabfd73575d";
    hamcrest      = "7215234e14a7c82458829c542edbf2899ceedbd3";
    lhttpc        = "3c7fdeee241b6813efddcb08ad1697186780d385";
    mochijson2    = "3663fb01fd98958181adc2d1300c7bfa553e1434";
    mochijson3    = "1a1c913ac80bb45d3de5fbd74d21e96c45e9e844";
    mustache      = "c0154ce140d7c5b088eee6aaa05226b388583ef4";
    p1_cache_tab  = "7b89d6afb66d8ff9d56671864be74654f5b18e2f";
    p1_stringprep = "9e9e0f8dbe6a70ef36e1d4436b458ca5a77fbcfb";
    p1_utils      = "9e646e4ff343e8e902410fa1fe28803202b7e340";
    proper        = "420c40dce61c5a0edd2e2dd7e72b7695e186b6a9";
    rebarFR       = "bcbf1ba233a5f8388f6d530c707d98db2021a48a";
    redo          = "7c7eaef4cd65271e2fc4ea88587e848407cf0762";
    reloader      = "9dd05d613c2abe563bc1c472950b96d2a832663b";
    usec          = "f85ffd8350d7000c26392c18bdfcdbb30f3b5ee8";
  };

  depMap = epkgs: with epkgs; {
    alarms       = [ folsom ];
    cowboy       = [ ranch cowlib ];
    ecoveralls   = [ jsx ];
    escalus      = [ exml fusco base16 lhttpc mustache wsecli ];
    espec        = [ reloader ];
    exometer     = [ lager parse_trans setup ];
    folsom       = [ bear meck ];
    katt         = [ mochijson3 lhttpc neotoma meck ];
    lager        = [ goldrush ];
    mustache     = [ meck ];
    p1_cache_tab = [ p1_utils ];
    pa           = [ proper ];
    parse_trans  = [ edown ];
    setup        = [ edown ];
    wsecli       = [ espec cucumberl hamcrest meck rebarFR wsock ];
  };

  shaSums = {
    alarms        = "1s5liz142srddkq2gd6s86hafl9mnridv8wri324gy9w2dn8iflk";
    base16        = "0kq6x40543sc2bkphj5pf83m9sc6knf5j83nihpp2x7wp6n704sk";
    bear          = "1x80qwyx56xclqhmcpdg082w1pbsw8jc9fa79hqy6q5i419w2wrg";
    cowboy        = "020as7fjjgl48g75q82z31fhw7pdnwyp0an788vfivjf0v6knakm";
    cowlib        = "1vrv5dgrqvdvm45g7chwmbfjyx9hd7wdk5fmzdlmv7zxagz0albc";
    cucumberl     = "0jby37zh7jzwv39fx2vh4cbi89syfilxdfx3qy7g9vjvmygzadrf";
    cuesport      = "0r89p9g5ps7rbd06rzzpyr7d3vm18bl9wpxy3sj2f551km248jbq";
    ecoveralls    = "0p5apdzfncn60rkg7lvn2dvkqh0jcqiq7ba177lvccw7grvmnd0s";
    edown         = "1gdbnmvnfpgyphdf04lidzmvcxwmsd01jinl9cmcc2fj2vjzp2ir";
    escalus       = "1ncwf95gf839bbpkdkm62rkiwgfcqiydyvblaks64gdsby8w7dlj";
    espec         = "1k070c54f6kcdk3ciipq9y651cmdci7g67kqmb4r1gib2y1apzad";
    exml          = "1qmixn7i4gvc080pvhy9c0pwlswyshnghwavg4y36x1sl8rhcv7g";
    exometer      = "1s3pr65b7sdp80hsq2y8pc578lmvxl2p0v8vppw1ivkvg4gvhgvs";
    folsom        = "0ybf0gj991s577i5jz05kshgv0x1k2llcyjq47p1mczd2kx5a3zi";
    fusco         = "05idlhxwlk5l0xni9fc52ncp3isin7k0gdzxzgifw1mk1157cg8g";
    goldrush      = "0fhi3jidn40gri49scvqvavqxh0ggfllx4xii8yqrs0l2l4lq9b5";
    hamcrest      = "1js9xmapavh50glfi062rlbg8nglq37gam6ahr9hq679bhsva3ka";
    idna          = "14daxbgpksqvnbx7mahfvh4mbzhza0yamlip9qqq14wgkdrihdmm";
    jsx           = "0kh1084ivva8wsl1545b0idd9g2r6q1ldf5cpgk2a36wmj2ni7zz";
    katt          = "0m7r99wbbcdbi666zsp40lmy39an5lhc3rgr1hcac161701dk6vw";
    lager         = "1bwd4g8bzh9msxzpp5nm4sssh883xycfrp76nk63r3vdah2ypmsv";
    lhttpc        = "0gzi5b99cd1zi31429j3zhq5k2c7f2175afaay7ai234gvj21g7d";
    meck          = "0s4qbvryap46cz63awpbv5zzmlcay5pn2lixgmgvcjarqv70cbs7";
    mochijson2    = "1ss9hyjbr1yk3khca30qh6xlrzcdm6jrmxyyx99vhbb7cg8l6k35";
    mochijson3    = "18dvmxzsqmjav30d7l6x3dslmvc4x5skhvmscv5ppymj0y0pw3hz";
    mustache      = "0nzna9jh5a0a6g738v04kq2vqxqvj5p7ngkngxqzrbmysrp8sz89";
    neotoma       = "0h4q5k8fb4l9iqkqsza8jpzg7pzprjx16pkfyz5fxbh02xig1r06";
    p1_cache_tab  = "1hw3hgzddcanzs6w88n66j2kdyz44zjayjwc3pg88bcr4rcwx46f";
    p1_stringprep = "0q6xkywanh2wjjr0601pqh63qm08bq1firap7n3sdcfh0h0d9vnx";
    p1_utils      = "0rlxgw4gsxacihlriv5spdnva88vygpx659m6x8bvqqmd6yhnpgr";
    pa            = "1kzh2g71sim98jd03xh697s8q0az0ma2p2inqc8cwhhr1lyfj2yp";
    parse_trans   = "0shvlxga5nwiqbpz9ibgs9p6zjcsl0kp3628ygd66hg1dpxjmyry";
    proper        = "1lva50nx6w1r1w4g9ln7ndmhms2zwwx9r6nrvcr0dkhmij2ynhh0";
    ranch         = "02b6nzdllrym90a5bhzlz4s52hyj9vwcn048na4j5jiivknm8g3r";
    rebarFR       = "17rkqx1cx8nvg9f0zy97d7xpy877bm68y37ywbmv5irj6kwbsqkk";
    recon         = "0gpj6i1a4gyqvwb4q8kkzc58n4n1v225pf47vbzab9arix6xj4ym";
    redo          = "14hg2jcs3qyl7aaz8ni9h8s97kjs0ksdfnh25m3hava7ga45jq1c";
    reloader      = "1ansv02klh9i53gmvxjk7vl83nsvyada58xn1pmc9gid0cl5vnl4";
    setup         = "0gyvfa84y6yz94bnybp7w2311gpgq69bazrmnpq43bd4s56dqlmj";
    usec          = "19xzx81lwgff45qic6508m7m5kkbm6s0glncqmzr9pbac2ikjlds";
    wsecli        = "010jv736h5f8w6n6zkz3s5cn4p6i54wvg7fnnk0db8m8lykzf9m2";
    wsock         = "1z8i4k8fja05s4pxy3y32hmc8mh1wc5s2i8gw5q3j0klci2sqsxq";
  };

  self = pkgs.lib.mapAttrs (n: f: f n) {
    alarms        = ghe "0.1.1"    "chrzaszcz";
    base16        = ghe "0.1"      "goj";
    bear          = ghe "0.1.3"    "boundary";
    cowboy        = ghe "1.0.1"    "ninenines";
    cowlib        = ghe "1.0.1"    "ninenines";
    cucumberl     = ghe "0.0.5"    "madtrick";
    cuesport      = ghe "0.1"      "goj";
    ecoveralls    = ghe "0.1"      "nifoc";
    edown         = ghe "0.7"      "uwiger";
    escalus       = ghe "2.6.1"    "esl";
    espec         = ghe "1"        "lucaspiller";
    exml          = ghe "2.1.5"    "esl";
    exometer      = ghe "1.1"      "Feuerlabs";
    folsom        = ghe "0.8.2"    "boundary";
    fusco         = ghe "0.0.0"    "esl";
    goldrush      = ghe "0.1.6"    "DeadZen";
    hamcrest      = ghe "0.1.0"    "hyperthunk";
    idna          = ghe "1.0.2"    "benoitc";
    jsx           = ghe "2.4.0"    "talentdeficit";
    katt          = ghe "1.3.0-rc" "for-GET";
    lager         = ghe "2.1.0"    "basho";
    lhttpc        = ghe "1.2.6"    "esl";
    meck          = ghe "0.8.2"    "eproxus";
    mochijson2    = ghe "0.1"      "bjnortier";
    mochijson3    = ghe "1.0"      "tophitpoker";
    mustache      = ghe "0.1.0"    "mojombo";
    neotoma       = ghe "1.7.2"    "seancribbs";
    p1_cache_tab  = ghe "0.1.0"    "processone";
    p1_stringprep = ghe "0.1.0"    "processone";
    p1_utils      = ghe "1"        "processone";
    pa            = ghe "0.2.0"    "lavrin";
    parse_trans   = ghe "2.9"      "uwiger";
    proper        = ghe "1.1"      "manopapad";
    ranch         = ghe "1.1.0"    "ninenines";
    rebarFR       = ghe "0.1"      "madtrick";
    recon         = ghe "2.2.1"    "ferd";
    redo          = ghe "1.1.0"    "jkvor";
    reloader      = ghe "1"        "lucaspiller";
    setup         = ghe "1.4"      "uwiger";
    usec          = ghe "0.1"      "esl";
    wsecli        = ghe "1.1.1"    "madtrick";
    wsock         = ghe "1.1.5"    "madtrick";
  };

in self
