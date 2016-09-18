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
    sd_notify.src.repo = "erlang-sd_notify";
    sd_notify.buildInputs = [ pkgs.systemd ];
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
    cucumberl     = "3f2cca66ed87a53a64177232428ffde606bdcb9a";
    cuesport      = "d82ff25baf393eb11712721fb1b881b4b9f80176";
    ecoveralls    = "40fa0d2f2057fff29e964f94fccf6ef2f13d34d2";
    espec         = "44dd72b8924425f09ad1093226ce0c755524a507";
    exometer      = "fade2105619d5f3237f2839176f2b219849065d6";
    fusco         = "0a428471aefb3a38207c2141249181e6c018e565";
    hamcrest      = "908a24fda4a46776a5135db60ca071e3d783f9f6";
    lhttpc        = "3c7fdeee241b6813efddcb08ad1697186780d385";
    mochijson2    = "e162b7f5be57b8a1fcf5cc99aa4948864af42bba";
    mochijson3    = "1a1c913ac80bb45d3de5fbd74d21e96c45e9e844";
    mustache      = "d0246fe143058b6404f66cf99fece3ff6e87b7ed";
    p1_cache_tab  = "7b89d6afb66d8ff9d56671864be74654f5b18e2f";
    p1_stringprep = "9e9e0f8dbe6a70ef36e1d4436b458ca5a77fbcfb";
    p1_utils      = "940f42ddfcdc0b7b2abf4d9ee292605a93699543";
    pa            = "070bf37fc8591de2cb8d975ab4935656f4110cb9";
    proper        = "20e62bc32f9bd43fe2ff52944a4ef99eb71d1399";
    rebarFR       = "bcbf1ba233a5f8388f6d530c707d98db2021a48a";
    redo          = "7c7eaef4cd65271e2fc4ea88587e848407cf0762";
    reloader      = "9dd05d613c2abe563bc1c472950b96d2a832663b";
    sd_notify     = "a1b6e244a122c4ccfa8d8e16ccd24ce1fd3109a2";
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
    base16        = "02qqbbw78llrihj4xpjvr77bh14s2jwzw46w6k8gfcfrv46pzgc3";
    bear          = "1x80qwyx56xclqhmcpdg082w1pbsw8jc9fa79hqy6q5i419w2wrg";
    cowboy        = "066hfi8pqs15cqh0qrzv2d2c17qyicv1y50ka8i9sw5fz9bn6aa3";
    cowlib        = "1jpy2ad33i22s0yfkkdq30n0hxqhdm847nki8bvziipsdg687a38";
    cucumberl     = "0jby37zh7jzwv39fx2vh4cbi89syfilxdfx3qy7g9vjvmygzadrf";
    cuesport      = "0sv7br7ixag3vliq3mg52zmy2im2pvplk4c21bnakjs9rfcgaisy";
    ecoveralls    = "0phyxiaf36fcnjagak81z0bh6mmdh1f8vba6acfbn9mx6c5ffa4l";
    edown         = "1gdbnmvnfpgyphdf04lidzmvcxwmsd01jinl9cmcc2fj2vjzp2ir";
    escalus       = "0k97zzkmnkksfihr0hy3sdy37w890cwilc9rps9ya9c1ga7yn717";
    espec         = "1k070c54f6kcdk3ciipq9y651cmdci7g67kqmb4r1gib2y1apzad";
    exml          = "1zl9wq3797m09gijb1wb545s4kif8q89ip5ym8hp5nlqjxnw8jmg";
    exometer      = "0fzr8c6ipc1zxqi00m407knqj2wlxhgcxdvkpbjrlnd04ja0gg1r";
    folsom        = "0ybf0gj991s577i5jz05kshgv0x1k2llcyjq47p1mczd2kx5a3zi";
    fusco         = "1zj5bd6z0lsvfpn58xjnwgwa0j13fkwcljmfhpiln7hwrl13qfnd";
    goldrush      = "0fhi3jidn40gri49scvqvavqxh0ggfllx4xii8yqrs0l2l4lq9b5";
    hamcrest      = "0irxidwrb37m0xwls6q9nn2zfs3pyxrgbnjgrhnh7gm35ib51hkj";
    idna          = "14daxbgpksqvnbx7mahfvh4mbzhza0yamlip9qqq14wgkdrihdmm";
    jsx           = "0kh1084ivva8wsl1545b0idd9g2r6q1ldf5cpgk2a36wmj2ni7zz";
    katt          = "0m7r99wbbcdbi666zsp40lmy39an5lhc3rgr1hcac161701dk6vw";
    lager         = "1bwd4g8bzh9msxzpp5nm4sssh883xycfrp76nk63r3vdah2ypmsv";
    lhttpc        = "0gzi5b99cd1zi31429j3zhq5k2c7f2175afaay7ai234gvj21g7d";
    meck          = "1mm83rvddj3f0h8gbj326x2hss91aqgasg387pn57wjacg8g3xf0";
    mochijson2    = "13s24yf73xff8b9vpwj7gxz9xy5yv50m62lgcsm09fk8jaas1z2k";
    mochijson3    = "18dvmxzsqmjav30d7l6x3dslmvc4x5skhvmscv5ppymj0y0pw3hz";
    mustache      = "0gpvy3dh4819ijlq57axzfdnjpp94xb1mnq87nq0jbiiy6gjsbvd";
    neotoma       = "0h4q5k8fb4l9iqkqsza8jpzg7pzprjx16pkfyz5fxbh02xig1r06";
    p1_cache_tab  = "1hw3hgzddcanzs6w88n66j2kdyz44zjayjwc3pg88bcr4rcwx46f";
    p1_stringprep = "0q6xkywanh2wjjr0601pqh63qm08bq1firap7n3sdcfh0h0d9vnx";
    p1_utils      = "0fppmby0wqnpzacl72b609i2iicb43f5dz95zpixh9740rk0s5c0";
    pa            = "060sdfv24xm8q5bjhq2jdz3k0h2p5fff349gm6cghg2xwfplaq2x";
    parse_trans   = "0shvlxga5nwiqbpz9ibgs9p6zjcsl0kp3628ygd66hg1dpxjmyry";
    proper        = "1nbs4h0y8h5rkxg1hlbbskgrwgyphkb0ajbyq2c85vd90ndhamr8";
    ranch         = "0ybx1wszkds5hbcw05rwmj62aln8n2rcd2pxz43rc9c9yxnj26cc";
    rebarFR       = "17rkqx1cx8nvg9f0zy97d7xpy877bm68y37ywbmv5irj6kwbsqkk";
    recon         = "0gpj6i1a4gyqvwb4q8kkzc58n4n1v225pf47vbzab9arix6xj4ym";
    redo          = "14hg2jcs3qyl7aaz8ni9h8s97kjs0ksdfnh25m3hava7ga45jq1c";
    reloader      = "1ansv02klh9i53gmvxjk7vl83nsvyada58xn1pmc9gid0cl5vnl4";
    setup         = "0gyvfa84y6yz94bnybp7w2311gpgq69bazrmnpq43bd4s56dqlmj";
    sd_notify     = "010ar96df8kn4by3kw25s454ghmjrfdrjql533d0kqi3dw8nvp8g";
    usec          = "19xzx81lwgff45qic6508m7m5kkbm6s0glncqmzr9pbac2ikjlds";
    wsecli        = "010jv736h5f8w6n6zkz3s5cn4p6i54wvg7fnnk0db8m8lykzf9m2";
    wsock         = "1z8i4k8fja05s4pxy3y32hmc8mh1wc5s2i8gw5q3j0klci2sqsxq";
  };

  self = pkgs.lib.mapAttrs (n: f: f n) {
    alarms        = ghe "0.1.1"    "chrzaszcz";
    base16        = ghe "1.0.0"    "goj";
    bear          = ghe "0.1.3"    "boundary";
    cowboy        = ghe "1.0.4"    "ninenines";
    cowlib        = ghe "1.0.2"    "ninenines";
    cucumberl     = ghe "0.0.5"    "madtrick";
    cuesport      = ghe "0.1"      "esl";
    ecoveralls    = ghe "0.1"      "nifoc";
    edown         = ghe "0.7"      "uwiger";
    escalus       = ghe "2.6.2"    "esl";
    espec         = ghe "1"        "lucaspiller";
    exml          = ghe "2.2.0"    "esl";
    exometer      = ghe "1.1"      "esl";
    folsom        = ghe "0.8.2"    "boundary";
    fusco         = ghe "0.0.0"    "esl";
    goldrush      = ghe "0.1.6"    "DeadZen";
    hamcrest      = ghe "0.1.0"    "hyperthunk";
    idna          = ghe "1.0.2"    "benoitc";
    jsx           = ghe "2.4.0"    "talentdeficit";
    katt          = ghe "1.3.0-rc" "for-GET";
    lager         = ghe "2.1.0"    "basho";
    lhttpc        = ghe "1.2.6"    "esl";
    meck          = ghe "0.8.3"    "eproxus";
    mochijson2    = ghe "0.1"      "bjnortier";
    mochijson3    = ghe "1.0"      "tophitpoker";
    mustache      = ghe "0.1.0"    "mojombo";
    neotoma       = ghe "1.7.2"    "seancribbs";
    p1_cache_tab  = ghe "0.1.0"    "processone";
    p1_stringprep = ghe "0.1.0"    "processone";
    p1_utils      = ghe "1"        "processone";
    pa            = ghe "0.2.0"    "erszcz";
    parse_trans   = ghe "2.9"      "uwiger";
    proper        = ghe "1.1"      "manopapad";
    ranch         = ghe "1.2.1"    "ninenines";
    rebarFR       = ghe "0.1"      "madtrick";
    recon         = ghe "2.2.1"    "ferd";
    redo          = ghe "1.1.0"    "jkvor";
    reloader      = ghe "1"        "lucaspiller";
    setup         = ghe "1.4"      "uwiger";
    sd_notify     = ghe "1"        "systemd";
    usec          = ghe "0.1"      "esl";
    wsecli        = ghe "1.1.1"    "madtrick";
    wsock         = ghe "1.1.5"    "madtrick";
  };

in self
