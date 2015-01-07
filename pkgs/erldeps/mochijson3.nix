{ buildErlang, fetchFromGitHub }:

buildErlang {
  name = "mochijson3";
  version = "1.0";

  src = fetchFromGitHub {
    repo = "mochijson3";
    owner = "tophitpoker";
    rev = "1a1c913ac80bb45d3de5fbd74d21e96c45e9e844";
    sha256 = "18dvmxzsqmjav30d7l6x3dslmvc4x5skhvmscv5ppymj0y0pw3hz";
  };
}
