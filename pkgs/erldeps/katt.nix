{ buildErlang, fetchFromGitHub, mochijson3, lhttpc, neotoma, meck }:

buildErlang rec {
  name = "katt";
  version = "1.3.0-rc";

  src = fetchFromGitHub {
    owner = "for-GET";
    repo = "katt";
    rev = version;
    sha256 = "0m7r99wbbcdbi666zsp40lmy39an5lhc3rgr1hcac161701dk6vw";
  };

  postPatch = ''
    patchShebangs priv/compile-parser
  '';

  erlangDeps = [ mochijson3 lhttpc neotoma meck ];
}
