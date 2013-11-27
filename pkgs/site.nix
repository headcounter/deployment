{ stdenv, fetchgit, haxe }:

let
  hase = fetchgit {
    url = "git://github.com/aszlig/hase.git";
    rev = "e2c4f105f7f74651b81b48400a120246acecfe0a";
    sha256 = "1f3z0n3bswd18wxmgy4n8ygb0yz54zla5qyhp9z885mg41pxyl6w";
  };
in stdenv.mkDerivation {
  name = "headcounter-site";

  src = ./site;

  buildInputs = [ haxe ];

  buildPhase = ''
    haxe -cp . -main Headcounter -cp "${hase}" -js headcounter.js -dce full
    cat > index.html <<HTML
    <!DOCTYPE html>
    <title>Headcounter - coming soon</title>
    <meta charset="UTF-8">
    <script src="headcounter.js"></script>
    HTML
  '';

  installPhase = ''
    ensureDir "$out"
    cp -t "$out" headcounter.js index.html
  '';
}
