{ buildErlang, fetchFromGitHub }:

buildErlang rec {
  name = "cowlib";
  version = "1.0.1";

  src = fetchFromGitHub {
    repo = "cowlib";
    owner = "ninenines";
    rev = version;
    sha256 = "1vrv5dgrqvdvm45g7chwmbfjyx9hd7wdk5fmzdlmv7zxagz0albc";
  };
}
