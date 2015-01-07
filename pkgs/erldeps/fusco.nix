{ buildErlang, fetchFromGitHub }:

buildErlang rec {
  name = "fusco";
  version = "0.0.0";

  src = fetchFromGitHub {
    owner = "esl";
    repo = "fusco";
    rev = "78650a15cf244065ab3ee74dafb8efabfd73575d";
    sha256 = "05idlhxwlk5l0xni9fc52ncp3isin7k0gdzxzgifw1mk1157cg8g";
  };
}
