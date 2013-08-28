{ resources, ... }:
{
  users.extraUsers.hydrabuild = {
    description = "Hydra build user";
    group = "users";
    home = "/home/hydrabuild";
    useDefaultShell = true;
    createHome = true;
    isSystemUser = false;
    openssh.authorizedKeys.keys = [
      resources.sshKeyPairs."hydra-build".public_key
    ];
  };
}
