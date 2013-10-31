{ pkgs, config, ... }:

with pkgs.lib;

let
  genSSLVHost = vhost: configuration: let
    genConf = sock: {
      type = "static";
      on = vhost.fqdn;
      socket = "${sock}:443";

      socketConfig = ''
        ssl.engine      = "enable"
        ssl.pemfile     = "${vhost.ssl.privateKey.path}"
        ssl.ca-file     = "${vhost.ssl.intermediateCert}"
      '';

      inherit configuration;
    };
  in [ (genConf vhost.ipv4) (genConf "[${vhost.ipv6}]") ];
in {
  imports = [ ../hydra.nix ../lighttpd.nix ../domains.nix ];

  services.headcounter.lighttpd = {
    enable = true;

    # only listen internally
    defaultPort = 80;
    configuration = ''
      server.bind = "127.0.0.1"
    '';

    modules.proxy.enable = true;
    modules.magnet.enable = true;
    modules.setenv.enable = true;

    virtualHosts = with config.vhosts; genSSLVHost headcounter ''
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
}
