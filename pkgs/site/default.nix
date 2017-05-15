{ stdenv, fetchFromGitHub, haxe }:

let
  hase = stdenv.mkDerivation {
    name = "hase";
    src = fetchFromGitHub {
      owner = "aszlig";
      repo = "hase";
      rev = "6ecd0edebaefbf434429325597edce81fd19ec4e";
      sha256 = "0awzw0ajk0dkp90jkvlfg9700jxq996p54321499bsrppr864a3l";
    };
    installPhase = ''
      mkdir -p "$out/lib/haxe/hase"
      mv * "$out/lib/haxe/hase/"
    '';
  };

in stdenv.mkDerivation {
  name = "headcounter-site";

  src = ./frontend;

  buildInputs = [ haxe hase ];

  buildPhase = ''
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
    mkdir "$out"
    cp -t "$out" headcounter.js index.html
  '';
}
