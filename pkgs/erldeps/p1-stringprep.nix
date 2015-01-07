{ buildErlang, fetchFromGitHub }:

buildErlang {
  name = "p1_stringprep";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "processone";
    repo = "stringprep";
    rev = "9e9e0f8dbe6a70ef36e1d4436b458ca5a77fbcfb";
    sha256 = "0q6xkywanh2wjjr0601pqh63qm08bq1firap7n3sdcfh0h0d9vnx";
  };
}
