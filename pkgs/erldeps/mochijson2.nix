{ buildErlang, fetchgit }:

buildErlang {
  name = "mochijson2";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/bjnortier/mochijson2.git";
    rev = "3663fb01fd98958181adc2d1300c7bfa553e1434";
    sha256 = "1ss9hyjbr1yk3khca30qh6xlrzcdm6jrmxyyx99vhbb7cg8l6k35";
  };
}
