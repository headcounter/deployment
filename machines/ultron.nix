{ pkgs, config, ... }:

with pkgs.lib;

{
  imports = [ ../hydra.nix ../lighttpd.nix ../domains.nix ];

  services.headcounter.lighttpd = {
    enable = true;
    modules.proxy.enable = true;
    modules.magnet.enable = true;
    modules.setenv.enable = true;

    virtualHosts = singleton {
      type = "static";
      on = "headcounter.org";
      socket = ":443";

      socketConfig = with config.vhosts.headcounter.ssl; ''
        server.use-ipv6 = "enable"
        ssl.engine      = "enable"
        ssl.pemfile     = "${privateKey.path}"
        ssl.ca-file     = "${intermediateCert}"
      '';

      configuration = ''
        $HTTP["url"] =~ "^/hydra(?:$|/)" {
          magnet.attract-physical-path-to = ( "${pkgs.writeText "rewrite.lua" ''
          if string.sub(lighty.env["request.uri"], 1, 6) == "/hydra" then
            lighty.env["request.uri"] = string.sub(lighty.env["request.uri"], 7)
          end
          ''}" )
          setenv.add-request-header = (
            "X-Request-Base" => "https://headcounter.org/hydra/"
          )
          proxy.balance = "hash"
          proxy.server = ("/hydra" => ((
            "host" => "127.0.0.1",
            "port" => 3000
          )))
        }
      '';
    };
  };
}
