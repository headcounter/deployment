{ buildErlang, fetchFromGitHub }:

buildErlang rec {
  name = "goldrush";
  version = "0.1.6";

  src = fetchFromGitHub {
    owner = "DeadZen";
    repo = "goldrush";
    rev = version;
    sha256 = "0fhi3jidn40gri49scvqvavqxh0ggfllx4xii8yqrs0l2l4lq9b5";
  };
}
