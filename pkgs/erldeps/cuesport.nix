{ buildErlang, fetchgit }:

buildErlang {
  name = "cuesport";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/goj/cuesport.git";
    rev = "3b16d99d8bc41774dbc4dabad8054b4423dec5a6";
    sha256 = "0al9wch2ypwpw3wqbk0sk6bjb232nzh37n5v4niy4ws5a51hm78c";
  };
}
