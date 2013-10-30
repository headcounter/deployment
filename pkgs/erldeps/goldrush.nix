{ buildErlang, fetchgit }:

buildErlang {
  name = "goldrush";
  version = "0.1.2";

  src = fetchgit {
    url = "https://github.com/DeadZen/goldrush.git";
    rev = "879c69874a555b2c13498aa4a3da6f6bbbb6a031";
    sha256 = "0n14csjn3m6bddfayv37x53rydhmb8aabx7qbh1kc4mv12rx15yf";
  };
}
