{ pkgs, config, ... }:

with pkgs.lib;

let
  genSSLVHost = vhost: configuration: let
    genConf = sock: {
      type = "static";
      on = vhost.fqdn;
      socket = "${sock}:443";

      socketConfig = ''
        ssl.engine  = "enable"
        ssl.pemfile = "${vhost.ssl.privateKey.path}"
        ssl.ca-file = "${vhost.ssl.intermediateCert}"
      '';

      inherit configuration;
    };
  in [ (genConf vhost.ipv4) (genConf "[${vhost.ipv6}]") ];
in {
  imports = [ ../hydra.nix ../domains.nix ../chromium.nix ];

  deployment.hetzner.partitions = ''
    clearpart --all --initlabel --drives=sda,sdb

    part swap1 --recommended --label=swap1 --fstype=swap --ondisk=sda
    part swap2 --recommended --label=swap2 --fstype=swap --ondisk=sdb

    part btrfs.1 --grow --ondisk=sda
    part btrfs.2 --grow --ondisk=sdb

    btrfs / --data=1 --metadata=1 --label=root btrfs.1 btrfs.2
  '';

  boot.kernelPackages = pkgs.linuxPackages_3_10;

  fileSystems."/".options = concatStringsSep "," [
    "autodefrag"
    "space_cache"
    "inode_cache"
    "compress=lzo"
    "noatime"
  ];

  services.headcounter.lighttpd = {
    enable = true;

    modules.proxy.enable = true;
    modules.magnet.enable = true;
    modules.setenv.enable = true;
    modules.redirect.enable = true;

    virtualHosts = with config.headcounter.vhosts; genSSLVHost headcounter ''
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
      } # http://redmine.lighttpd.net/issues/1268
      else $HTTP["url"] =~ "" {
        url.redirect = ( "^/(.*)" => "https://jabber.headcounter.org/$1" )
      }
    '' ++ singleton {
      socket = ":80";
      socketConfig = ''
        url.redirect = ( "^/(.*)" => "https://jabber.headcounter.org/$1" )
      '';
    };
  };
}
