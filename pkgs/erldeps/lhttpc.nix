{ buildErlang, fetchgit }:

buildErlang rec {
  name = "lhttpc";
  version = "1.2.6";

  src = fetchgit {
    url = "https://github.com/esl/lhttpc.git";
    rev = "3c7fdeee241b6813efddcb08ad1697186780d385";
    sha256 = "01fxdrcpgsi7iq7pjagzvkp5vspsp9fbvjwmqfhnb9mksf3pxryc";
  };
}
