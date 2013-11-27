{ stdenv, fetchgit, fetchurl, srcOnly, makeWrapper, unzip, haxe, neko }:

let
  hase = fetchgit {
    url = "git://github.com/aszlig/hase.git";
    rev = "e2c4f105f7f74651b81b48400a120246acecfe0a";
    sha256 = "1f3z0n3bswd18wxmgy4n8ygb0yz54zla5qyhp9z885mg41pxyl6w";
  };

  hxcpp = srcOnly rec {
    name = "hxcpp-3.0.2";
    src = fetchurl {
      name = "${name}.zip";
      url = "http://lib.haxe.org/files/3.0/hxcpp-3,0,2.zip";
      sha256 = "1wbx6m0cqz2zwg2dkf53czpwvfww0cdjmv9a61qsfrshjpss8dn9";
    };
    buildInputs = [ unzip ];
  };
in stdenv.mkDerivation {
  name = "headcounter-site";

  src = ./site;

  buildInputs = [ haxe neko makeWrapper ];

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
    haxe -main Headcounter -lib hase -js headcounter.js -dce full

    cat > index.html <<HTML
    <!DOCTYPE html>
    <title>Headcounter - coming soon</title>
    <meta charset="UTF-8">
    <script src="headcounter.js"></script>
    <noscript>
    Please either <a href="https://jabber.headcounter.org/">continue here</a> or
    enable JavaScript.
    </noscript>
    HTML
  '';

  installPhase = ''
    ensureDir "$out/bin" "$html"
    install build/Headcounter "$out/bin/headcounter"
    cp -t "$html" headcounter.js index.html
    wrapProgram "$out/bin/headcounter" \
      --set LD_LIBRARY_PATH "${hxcpp}/bin/Linux64"
  '';
}
