{ pkgs, config, ... }:

with pkgs.lib;

{
  require = [ ../hydra.nix ../lighttpd.nix ../domains.nix ];

  services.headcounter.lighttpd = {
    enable = true;
    modules.proxy.enable = true;

    virtualHosts = singleton {
      type = "static";
      on = "hydra.headcounter.org";
      configuration = ''
        proxy.balance = "hash"
        proxy.server = ("" => ((
          "host" => "127.0.0.1",
          "port" => 3000
        )))
      '';
    };
  };
}
