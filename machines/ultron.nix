{ pkgs, config, ... }:

with pkgs.lib;

{
  require = [ ../hydra.nix ../lighttpd.nix ../subnets.nix ];

  headcounter.subnets.eth0 = {
    prefix = 29;
    addrs = let
      ip = num: "78.47.32.${builtins.toString num}";
    in { # subnet 78.47.32.128/29
      https       = ip 128;
      jabber      = ip 129;
      headcounter = ip 130;
      aszlig      = ip 131;
      noicq       = ip 132;
      "no-icq"    = ip 133;
      nolabel1    = ip 134;
      nolabel2    = ip 135;
    };
  };

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
