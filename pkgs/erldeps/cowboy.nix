{ buildErlang, fetchFromGitHub, ranch, cowlib }:

buildErlang rec {
  name = "cowboy";
  version = "1.0.1";

  src = fetchFromGitHub {
    owner = "ninenines";
    repo = "cowboy";
    rev = version;
    sha256 = "020as7fjjgl48g75q82z31fhw7pdnwyp0an788vfivjf0v6knakm";
  };

  erlangDeps = [ ranch cowlib ];
}
