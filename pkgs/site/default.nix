{ stdenv, fetchFromGitHub, fetchurl, srcOnly, makeWrapper, unzip, haxe, neko }:

let
  hase = fetchFromGitHub {
    owner = "aszlig";
    repo = "hase";
    rev = "ad2107172a14bd4e024ca49b1bf285b13ab2fd56";
    sha256 = "1i29isgql7pi1qadchg2k10d3jqpfxhig0b2xfn4jm2ia69jyj2m";
  };

  hxcpp = srcOnly rec {
    name = "hxcpp-3.1.39";
    src = fetchurl {
      name = "${name}.zip";
      url = "http://lib.haxe.org/files/3.0/hxcpp-3,1,39.zip";
      sha256 = "0g1phsxrhy01gq9p5h61mibb6abr11h1i3zlbadrbswpglf7c8j2";
    };
    buildInputs = [ unzip ];
  };
in stdenv.mkDerivation {
  name = "headcounter-site";

  src = ./frontend;

  buildInputs = [ haxe neko ];

  outputs = [ "out" "html" ];

  buildPhase = ''
    export HOME=$(pwd)
    echo "$(pwd)" > .haxelib

    mkdir hase
    echo dev > hase/.current
    echo "${hase}" > hase/.dev

    mkdir hxcpp
    echo dev > hxcpp/.current
    echo "${hxcpp}" > hxcpp/.dev

    haxe -main Headcounter -cpp build -lib hase -dce full -D HXCPP_M64
    patchelf --set-rpath "${hxcpp}/bin/Linux64:${stdenv.cc.cc}/lib64" \
      build/Headcounter

    haxe -main Headcounter -lib hase -js headcounter.js -dce full

    cat > index.html <<HTML
    <!DOCTYPE html>
    <title>Headcounter - coming soon</title>
    <meta charset="UTF-8">
    <script src="headcounter.js"></script>
    </head>
    <noscript>
    Please either <a href="https://jabber.headcounter.org/">continue to the old
    site</a>, use <code>telnet headcounter.org</code> or enable JavaScript.
    </noscript>
    HTML
  '';

  installPhase = ''
    mkdir -p "$out/bin" "$html"
    install build/Headcounter "$out/bin/headcounter"
    cp -t "$html" headcounter.js index.html
  '';
}
