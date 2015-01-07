{ buildErlang, fetchFromGitHub }:

buildErlang rec {
  name = "proper";
  version = "1.1";

  src = fetchFromGitHub {
    owner = "manopapad";
    repo = "proper";
    rev = "v${version}";
    sha256 = "14g5sjspg0vc3jfr1m8pq6fsj3wlj72pad1qfw1w4vx87z15xzq6";
  };

  postPatch = ''
    patchShebangs write_compile_flags
  '';
}
