{ stdenv, fetchFromGitHub, haxe, haxePackages }:

let
  hase = stdenv.mkDerivation rec {
    name = "hase";
    version = "0.1.0";

    src = fetchFromGitHub {
      owner = "aszlig";
      repo = "hase";
      rev = "76704ba163c3878fc18590d895d1a5d9e16538dd";
      sha256 = "0f479fm38canqwjd8046k6l39pfln2gpm0anmn0pjjhg991svk4n";
    };

    installPhase = haxePackages.installLibHaxe {
      libname = "hase";
      inherit version;
      files = "hase";
    };
  };

in stdenv.mkDerivation {
  name = "headcounter-site";

  src = ./frontend;

  nativeBuildInputs = [ haxe ];
  buildInputs = [ hase ];

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
