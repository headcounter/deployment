{ buildGoPackage, fetchgit, fetchFromGitHub, libcap }:

let
  owner = "hlandau";
  repo = "acme";

in buildGoPackage rec {
  name = "acmetool-${version}";
  version = "0.0.59";

  buildInputs = [ libcap ];
  goPackagePath = "github.com/${owner}/${repo}";

  src = fetchFromGitHub {
    inherit owner repo;
    rev = "v${version}";
    sha256 = "1b7jybm5kxmypmhyx05kh65vxdqx7z8sarfs04v6sg2j0h6c2mig";
  };

  # This patch removes all the references to non-DNS responders.
  patches = [ ./remove-unneded-responders.patch ];

  # Remove the actual files that are non-DNS responders.
  postPatch = ''
    find responder -mindepth 1 -maxdepth 1 \( \
      -name dns.go -o -name responder.go \
    \) -o -delete
  '';

  extraSrcs = let
    mkDep = { goPackagePath, url ? "https://${goPackagePath}", rev, sha256 }: {
      inherit goPackagePath;
      src = fetchgit { inherit url rev sha256; };
    };
  in map mkDep [
    { goPackagePath = "github.com/alecthomas/template";
      rev = "a0175ee3bccc567396460bf5acd36800cb10c49c";
      sha256 = "0qjgvvh26vk1cyfq9fadyhfgdj36f1iapbmr5xp6zqipldz8ffxj";
    }
    { goPackagePath = "github.com/alecthomas/units";
      rev = "2efee857e7cfd4f3d0138cc3cbb1b4966962b93a";
      sha256 = "1j65b91qb9sbrml9cpabfrcf07wmgzzghrl7809hjjhrmbzri5bl";
    }
    { goPackagePath = "github.com/coreos/go-systemd";
      rev = "e97b35f834b17eaa82afe3d44715c34736bfa12b";
      sha256 = "091bplsq3cqw0b109kxwb372xz59x81crjg501mk4gdznvmg8jym";
    }
    { goPackagePath = "github.com/godbus/dbus";
      rev = "692d22898a1dffbb54a37706afcb1324c510f2ac";
      sha256 = "05x117pxxjbiyjyxx4p1ificmgdy94vldy812vbqxlc7sj6mkzzv";
    }
    { goPackagePath = "github.com/hlandau/buildinfo";
      rev = "337a29b5499734e584d4630ce535af64c5fe7813";
      sha256 = "1kq3r1i4rr9bcvj5yg8w1l95f6sfc3kn6kgcdmlh5i3j9w2sram8";
    }
    { goPackagePath = "github.com/hlandau/dexlogconfig";
      rev = "244f29bd260884993b176cd14ef2f7631f6f3c18";
      sha256 = "1d01ghx6xawj3nk3lpk51wbbpxdnc9vzvijvlayvp7cxgsacslbc";
    }
    { goPackagePath = "github.com/hlandau/goutils";
      rev = "0cdb66aea5b843822af6fdffc21286b8fe8379c4";
      sha256 = "0g83wn82g3vnndl9gp9i5jhlj6avih87xghqw1m8n9l6x82ypqkd";
    }
    { goPackagePath = "github.com/hlandau/xlog";
      rev = "197ef798aed28e08ed3e176e678fda81be993a31";
      sha256 = "08rjlqnjbfgpz5rbjq89l7y5vyhk99ivr928sqdi5gnqznbqs4m8";
    }
    { goPackagePath = "github.com/jmhodges/clock";
      rev = "880ee4c335489bc78d01e4d0a254ae880734bc15";
      sha256 = "1zzillkkkb3hrj13rg2g6f62216zc4n4f7j560f2491vcb371njg";
    }
    { goPackagePath = "github.com/mattn/go-isatty";
      rev = "dda3de49cbfcec471bd7a70e6cc01fcc3ff90109";
      sha256 = "1mz23fzi5rmcbz7nqijlivp1mfzivdb48rrab7h3zs81jvc5mi1m";
    }
    { goPackagePath = "github.com/mattn/go-runewidth";
      rev = "14207d285c6c197daabb5c9793d63e7af9ab2d50";
      sha256 = "0y6yq9zd4kh7fimnc00r3h9pr2pwa5j85b3jcn5dyfamsnm2xdsv";
    }
    { goPackagePath = "github.com/mitchellh/go-wordwrap";
      rev = "ad45545899c7b13c020ea92b2072220eefad42b8";
      sha256 = "0ny1ddngvwfj3njn7pmqnf3l903lw73ynddw15x8ymp7hidv27v9";
    }
    { goPackagePath = "github.com/ogier/pflag";
      rev = "45c278ab3607870051a2ea9040bb85fcb8557481";
      sha256 = "0620v75wppfd84d95n312wpngcb73cph4q3ivs1h0waljfnsrd5l";
    }
    { goPackagePath = "github.com/peterhellberg/link";
      rev = "3eea38ca14b7b3252feea5daf92be3864209eb1d";
      sha256 = "0zbkrnwlap7yhn2bks3xj772cmqhxvvz41dd9rkj00pm7r3lwk00";
    }
    { goPackagePath = "github.com/satori/go.uuid";
      rev = "b061729afc07e77a8aa4fad0a2fd840958f1942a";
      sha256 = "0q87n5an7ha2d8kl6gn9wi41rq0whsxq68w5x3nxz7w9vgkfnq1k";
    }
    { goPackagePath = "github.com/shiena/ansicolor";
      rev = "a422bbe96644373c5753384a59d678f7d261ff10";
      sha256 = "1dcn8a9z6a5dxa2m3fkppnajcls8lanbl38qggkf646yi5qsk1hc";
    }
    { goPackagePath = "golang.org/x/crypto";
      url = "https://go.googlesource.com/crypto";
      rev = "453249f01cfeb54c3d549ddb75ff152ca243f9d8";
      sha256 = "0akybbzgi3v507a39bgnkk79rfhj8gflr7538g5a0177z5i9ygwa";
    }
    { goPackagePath = "golang.org/x/net";
      url = "https://go.googlesource.com/net";
      rev = "906cda9512f77671ab44f8c8563b13a8e707b230";
      sha256 = "0aa33n5a2zzrm2pnjyc3xkdmf8hq2qpafgdp8v6fxfb0swqjl2n3";
    }
    { goPackagePath = "gopkg.in/alecthomas/kingpin.v2";
      rev = "e9044be3ab2a8e11d4e1f418d12f0790d57e8d70";
      sha256 = "1kzk1ylii9aqip0ss4zc2zxwa16lr5j3p3bgyjr276rig68i68ji";
    }
    { goPackagePath = "gopkg.in/cheggaaa/pb.v1";
      rev = "d7e6ca3010b6f084d8056847f55d7f572f180678";
      sha256 = "1n6r262dqcf3f3dd5h4shq9g1v7nci1rxpaishrj9nl68wl5a2dd";
    }
    { goPackagePath = "gopkg.in/hlandau/configurable.v1";
      rev = "41496864a1fe3e0fef2973f22372b755d2897402";
      sha256 = "0i9jbdvi8rz12xsrzzbfxg5lwsqakjcmccsa5a32asmv26k5byqa";
    }
    { goPackagePath = "gopkg.in/hlandau/easyconfig.v1";
      rev = "33e53e2d08656ccad000531debbf2656a896b695";
      sha256 = "0yxqh2bijrs0a8vg811dxhlnmnv09p72v8gxja4044fh76r4142g";
    }
    { goPackagePath = "gopkg.in/hlandau/service.v2";
      rev = "b64b3467ebd16f64faec1640c25e318efc0c0d7b";
      sha256 = "0lpx88f46ylx9lf6jgwcjgklll1pc1mlakrywpa0wzhjj7a4jinc";
    }
    { goPackagePath = "gopkg.in/hlandau/svcutils.v1";
      rev = "c25dac49e50cbbcbef8c81b089f56156f4067729";
      sha256 = "12b6p71mk33r44d71xizjq82fls0ykfwfl5gnmckbgpxms4bj2xf";
    }
    { goPackagePath = "gopkg.in/square/go-jose.v1";
      rev = "aa2e30fdd1fe9dd3394119af66451ae790d50e0d";
      sha256 = "0drajyadd6c4m5qv0jxcv748qczg8sgxz28nva1jn39f234m02is";
    }
    { goPackagePath = "gopkg.in/tylerb/graceful.v1";
      rev = "4654dfbb6ad53cb5e27f37d99b02e16c1872fbbb";
      sha256 = "1qspbrzr3h6c89v75c99avn7iizkfnjh901wp650vyy0j3p6ydnd";
    }
    { goPackagePath = "gopkg.in/yaml.v2";
      rev = "a3f3340b5840cee44f372bddb5880fcbc419b46a";
      sha256 = "1djb53a8ikwgkfpf8namgf4d8pq1mq6q9q2c7q0z8x4dxf3whxj7";
    }
  ];
}
