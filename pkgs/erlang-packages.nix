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
    escalus.postPatch = ''
      sed -i -e 's/^\(-define(WAIT_FOR_STANZA_TIMEOUT, *\)[0-9]\+/\110000/' \
        src/escalus_client.erl
    '';
    exml.buildInputs = [ pkgs.expat ];
    exometer.EXOMETER_PACKAGES = "(minimal)";
    exometer.postPatch = ''
      # "lager" is needed to be in the Erlang search path in order to apply the
      # "lager_transform" parser transformation.
      sed -i \
        -e '/^{deps/s!\[!&{lager, ".*", not_needed},!' \
        -e '/^{mandatory_deps/s!\[!&lager,!' \
        rebar.config
    '';
    fast_tls.buildInputs = [ pkgs.openssl ];
    fast_tls.patches = [ mongooseim/dhparams.patch ];
    hamcrest.src.repo = "hamcrest-erlang";
    idna.src.repo = "erlang-idna";
    katt.KATT_BARE_MODE = true;
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
    alarms        = "eb87b3294be4d7e96d327c57b3d6f9db851c4579";
    base16        = "f78918e7b593fbdc35ec9bcc349aa50f47f45a8b";
    cucumberl     = "3f2cca66ed87a53a64177232428ffde606bdcb9a";
    cuesport      = "d82ff25baf393eb11712721fb1b881b4b9f80176";
    ecoveralls    = "0e52c4709f763d512e6972e91330977cfedb3d13";
    erlsh         = "2fce5134f969be3b92ec501d9caa17e4122ba9aa";
    espec         = "44dd72b8924425f09ad1093226ce0c755524a507";
    exometer      = "fa4f539823b42744bbb9cd42b22ec71305ce4b8f";
    exometer_core = "9e4e4256e71e313070996a9d8e3fc4e270820f4e";
    fusco         = "0a428471aefb3a38207c2141249181e6c018e565";
    hamcrest      = "908a24fda4a46776a5135db60ca071e3d783f9f6";
    lasse         = "692eaec1a226a6247f8450fc23b86b8afc1cee7a";
    lhttpc        = "3c7fdeee241b6813efddcb08ad1697186780d385";
    mochijson2    = "e162b7f5be57b8a1fcf5cc99aa4948864af42bba";
    mochijson3    = "1a1c913ac80bb45d3de5fbd74d21e96c45e9e844";
    mustache      = "031c7aa302810424c198aa003aa8cf090150b555";
    pa            = "e53c540005f4114a854cfa414b00cbc7e9e65ecb";
    proper        = "20e62bc32f9bd43fe2ff52944a4ef99eb71d1399";
    quickrand     = "0b0d0089802231b4d65bd1809a74d24a1b64da22";
    rebarFR       = "bcbf1ba233a5f8388f6d530c707d98db2021a48a";
    redo          = "7c7eaef4cd65271e2fc4ea88587e848407cf0762";
    reloader      = "9dd05d613c2abe563bc1c472950b96d2a832663b";
    sd_notify     = "fcc0bd000c1396f14a9a40c2916bc673f46755ff";
    shotgun       = "4e670657b24b55a826bdb3d2d93e56222ab79e67";
    usec          = "f85ffd8350d7000c26392c18bdfcdbb30f3b5ee8";
    uuid          = "6f5d9e3b8a016013e308dd7855273f5ac19ab720";
  };

  depMap = epkgs: with epkgs; {
    alarms        = [ folsom ];
    cache_tab     = [ p1_utils ];
    cowboy        = [ ranch cowlib ];
    ecoveralls    = [ jsx ];
    escalus       = [ exml fusco base16 lhttpc mustache wsecli ];
    espec         = [ reloader ];
    exometer      = [ exometer_core lager parse_trans setup ];
    exometer_core = [ folsom lager meck parse_trans setup ];
    fast_tls      = [ p1_utils ];
    folsom        = [ bear meck ];
    gun           = [ cowlib ranch ];
    katt          = [ mochijson3 lhttpc neotoma meck ];
    lager         = [ goldrush ];
    mustache      = [ meck ];
    pa            = [ proper ];
    parse_trans   = [ edown ];
    setup         = [ edown ];
    shotgun       = [ gun ];
    stringprep    = [ p1_utils ];
    uuid          = [ quickrand ];
    wsecli        = [ espec cucumberl hamcrest meck rebarFR wsock ];
  };

  shaSums = {
    alarms        = "1zas2ngdzvanivnl6q7569gawnivm45nldrqyighbca0cj8xn1nw";
    base16        = "02qqbbw78llrihj4xpjvr77bh14s2jwzw46w6k8gfcfrv46pzgc3";
    bear          = "1x80qwyx56xclqhmcpdg082w1pbsw8jc9fa79hqy6q5i419w2wrg";
    cache_tab     = "0jnhvclwnafdq0wrsbddbrvsh1ixf64y4yxy4shsymsn3i9vif0s";
    cowboy        = "066hfi8pqs15cqh0qrzv2d2c17qyicv1y50ka8i9sw5fz9bn6aa3";
    cowlib        = "1jpy2ad33i22s0yfkkdq30n0hxqhdm847nki8bvziipsdg687a38";
    cucumberl     = "0jby37zh7jzwv39fx2vh4cbi89syfilxdfx3qy7g9vjvmygzadrf";
    cuesport      = "0sv7br7ixag3vliq3mg52zmy2im2pvplk4c21bnakjs9rfcgaisy";
    ecoveralls    = "0p5apdzfncn60rkg7lvn2dvkqh0jcqiq7ba177lvccw7grvmnd0s";
    edown         = "15y2awqr18s7104g672nxv46p35jc1yf8q6gr4yn73515lws1jw1";
    erlsh         = "0dfk5ng1zn8hairyzaxxfwai17637acdsdigy47f1gd43h8ibbi2";
    escalus       = "1xhxxxwbjsbzqm2598wkjv4y2q6ykv2q1m6m3b30lxqni4gfhc60";
    espec         = "1k070c54f6kcdk3ciipq9y651cmdci7g67kqmb4r1gib2y1apzad";
    exml          = "0y4qashnwznqk2hsg2kbz8hgqmc1ki4ry90nq0nhc46scrsnb9f7";
    exometer      = "1x3rq3z9647802jhl5504s84z2vy1ghnqh8b89ax1s389v9r72af";
    exometer_core = "1vg6m9llq29wag43pxdm61jgbavms0nwi7w1kbr3i7rj8iyf60hx";
    fast_tls      = "0a8axj97d1lyqmj4mx01h89ldga1pfih28ljkajapycny8q8hhxk";
    folsom        = "0ybf0gj991s577i5jz05kshgv0x1k2llcyjq47p1mczd2kx5a3zi";
    fusco         = "1zj5bd6z0lsvfpn58xjnwgwa0j13fkwcljmfhpiln7hwrl13qfnd";
    goldrush      = "0611dgqa7bv9varr2l7whmj6x4x1xa7i544idk6wqaay4hpa7fs7";
    gun           = "12fpq49m2lk8cds77vl30va82gjr11ywdzqx281qr57hwprn5vmk";
    hamcrest      = "0irxidwrb37m0xwls6q9nn2zfs3pyxrgbnjgrhnh7gm35ib51hkj";
    idna          = "1iq58r1srawyz9qpx5pngh4w7p6icj5xg07m1fmivgbqi1g9rz9z";
    jiffy         = "07khkl0nxikq50v16029hvlxcr67r4ya85mfldk9m78849mk02ak";
    jsx           = "0kh1084ivva8wsl1545b0idd9g2r6q1ldf5cpgk2a36wmj2ni7zz";
    katt          = "1jjgk356rvkqyccrjnl4d76lj3dirj7p4h6i6zialw4xav8njdny";
    lager         = "14bax07wrvfms4zlhz8ay8565pjji01w7qiq6b93i787a484viph";
    lasse         = "0xjgv1fn0fs3zfxq6p003f7ibhalvi8ws80r98xpg2yawjbxc2r6";
    lhttpc        = "0gzi5b99cd1zi31429j3zhq5k2c7f2175afaay7ai234gvj21g7d";
    meck          = "1mm83rvddj3f0h8gbj326x2hss91aqgasg387pn57wjacg8g3xf0";
    mochijson2    = "13s24yf73xff8b9vpwj7gxz9xy5yv50m62lgcsm09fk8jaas1z2k";
    mochijson3    = "18dvmxzsqmjav30d7l6x3dslmvc4x5skhvmscv5ppymj0y0pw3hz";
    mustache      = "0p4ay88s5hzkdl8k1v8pyzg725g764pgf0p5wxpgbkwp37q2gx5m";
    neotoma       = "0h4q5k8fb4l9iqkqsza8jpzg7pzprjx16pkfyz5fxbh02xig1r06";
    p1_pgsql      = "1gbyrya07f3liqc8m1llxqkghb94653hrlcmsc0hx9dmdi4ryqs3";
    p1_utils      = "16fpsblnrvj8lg8g1h699m8smin4xs4qhmvl7m606fhlgjdhmxl8";
    pa            = "07i4rkwc5fw6jq9sxrvkrrddgrr0gbpm4bdn9c84yk36inhd4fy1";
    parse_trans   = "0shvlxga5nwiqbpz9ibgs9p6zjcsl0kp3628ygd66hg1dpxjmyry";
    poolboy       = "0dswsbswfx0mrnxqnjkxc4izjjzia39sjxrlx4gxc2b468m791cr";
    proper        = "1nbs4h0y8h5rkxg1hlbbskgrwgyphkb0ajbyq2c85vd90ndhamr8";
    quickrand     = "182bi6c2fs0dh3imb92jj85za0ajzp4bqsvjrv9abcqphzch5h2b";
    ranch         = "1zbp7963qwh7433sk5bqnh0fzhj9f7dwnwgjij05s8f1qqih41p7";
    rebarFR       = "17rkqx1cx8nvg9f0zy97d7xpy877bm68y37ywbmv5irj6kwbsqkk";
    recon         = "1h2w1ndangry7lxjg29lma3dar25qq4zwhj7dlbfc8b8vj6pxsjr";
    redo          = "14hg2jcs3qyl7aaz8ni9h8s97kjs0ksdfnh25m3hava7ga45jq1c";
    reloader      = "1ansv02klh9i53gmvxjk7vl83nsvyada58xn1pmc9gid0cl5vnl4";
    sd_notify     = "04fzq9hyyfy2ypzq9j3ldfhid2n6l58wymb2lz4i7g8zi3g3fn2b";
    setup         = "00g64hc1920hs7jc070x75pz0jf8wrqwhsqqgymb7w8yc4lw35zc";
    shotgun       = "1d6spjqvsj8vkbw3bjzga2ywjdqvfxascfnh89wc0j1hx8yvs9s0";
    stringprep    = "1x8fsad3birp2xd9s7sgqi0dzkx1ddi1xx4rdrsbp79j12lvd52l";
    usec          = "19xzx81lwgff45qic6508m7m5kkbm6s0glncqmzr9pbac2ikjlds";
    uuid          = "1n7r9lzxbjbglzy323blalimh2sbih1hdmn4rwcd9hcp34w8c7w9";
    wsecli        = "010jv736h5f8w6n6zkz3s5cn4p6i54wvg7fnnk0db8m8lykzf9m2";
    wsock         = "1z8i4k8fja05s4pxy3y32hmc8mh1wc5s2i8gw5q3j0klci2sqsxq";
  };

  self = pkgs.lib.mapAttrs (n: f: f n) {
    alarms        = ghe "0.1"         "chrzaszcz";
    base16        = ghe "1.0.0"       "goj";
    bear          = ghe "0.1.3"       "boundary";
    cache_tab     = ghe "1.0.4"       "processone";
    cowboy        = ghe "1.0.4"       "ninenines";
    cowlib        = ghe "1.0.2"       "ninenines";
    cucumberl     = ghe "0.0.5"       "madtrick";
    cuesport      = ghe "0.1"         "esl";
    ecoveralls    = ghe "0.1"         "nifoc";
    edown         = ghe "0.8"         "uwiger";
    erlsh         = ghe "0.1.0"       "proger";
    escalus       = ghe "3.0.1"       "esl";
    espec         = ghe "1"           "lucaspiller";
    exml          = ghe "2.4.1"       "esl";
    exometer      = ghe "1.2.1"       "esl";
    exometer_core = ghe "1.4"         "esl";
    fast_tls      = ghe "1.0.7"       "processone";
    folsom        = ghe "0.8.2"       "boundary";
    fusco         = ghe "0.0.0"       "esl";
    goldrush      = ghe "0.1.9"       "DeadZen";
    gun           = ghe "1.0.0-pre.1" "ninenines";
    hamcrest      = ghe "0.1.0"       "hyperthunk";
    idna          = ghe "1.1.0"       "benoitc";
    jiffy         = ghe "0.14.8"      "davisp";
    jsx           = ghe "2.4.0"       "talentdeficit";
    katt          = ghe "1.5.2"       "for-GET";
    lager         = ghe "3.2.4"       "basho";
    lasse         = ghe "1.1.0"       "inaka";
    lhttpc        = ghe "1.2.6"       "esl";
    meck          = ghe "0.8.3"       "eproxus";
    mochijson2    = ghe "0.1"         "bjnortier";
    mochijson3    = ghe "1.0"         "tophitpoker";
    mustache      = ghe "0.1.0"       "mojombo";
    neotoma       = ghe "1.7.2"       "seancribbs";
    p1_pgsql      = ghe "1.1.1"       "processone";
    p1_utils      = ghe "1.0.5"       "processone";
    pa            = ghe "0.2.0"       "erszcz";
    parse_trans   = ghe "2.9"         "uwiger";
    poolboy       = ghe "1.5.1"       "devinus";
    proper        = ghe "1.1"         "manopapad";
    quickrand     = ghe "1.5.2"       "okeuday";
    ranch         = ghe "1.3.0"       "ninenines";
    rebarFR       = ghe "0.1"         "madtrick";
    recon         = ghe "2.3.2"       "ferd";
    redo          = ghe "1.1.0"       "jkvor";
    reloader      = ghe "1"           "lucaspiller";
    sd_notify     = ghe "1"           "systemd";
    setup         = ghe "1.8.0"       "uwiger";
    shotgun       = ghe "0.3.0"       "inaka";
    stringprep    = ghe "1.0.6"       "processone";
    usec          = ghe "0.1"         "esl";
    uuid          = ghe "1.5.2"       "okeuday";
    wsecli        = ghe "1.1.1"       "madtrick";
    wsock         = ghe "1.1.5"       "madtrick";
  };

in self
