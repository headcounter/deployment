{ buildErlang, fetchgit, folsom }:

buildErlang {
  name = "alarms";
  version = "0.1";

  src = fetchgit {
    url = "https://github.com/chrzaszcz/alarms.git";
    rev = "dcd64216c5fffcdde80e779b161f195a03294a8a";
    sha256 = "05h0s3brydxnjzpc39d5fy8n1in3v8y87n8mzix3zcqr9fknh8xg";
  };

  erlangDeps = [ folsom ];
}
