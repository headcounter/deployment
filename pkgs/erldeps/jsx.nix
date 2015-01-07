{ buildErlang, fetchFromGitHub }:

buildErlang rec {
  name = "jsx";
  version = "2.4.0";

  src = fetchFromGitHub {
    repo = "jsx";
    owner = "talentdeficit";
    rev = "v${version}";
    sha256 = "0kh1084ivva8wsl1545b0idd9g2r6q1ldf5cpgk2a36wmj2ni7zz";
  };
}
