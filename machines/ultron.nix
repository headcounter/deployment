{ pkgs, config, nodes, ... }:

with pkgs.lib;

let
  ownpkgs = import ../pkgs { inherit pkgs; };
  mainSite = ownpkgs.site;

  hydraIPv4 = nodes.taalo.config.networking.p2pTunnels.ssh.ultron.localIPv4;

  genIPv46VHosts = vhost: scfg: let
    getSocketPort = s: last (splitString ":" s);
    processNode = addr: node: node // {
      socket = "${addr}:${getSocketPort (node.socket or "80")}";
    };
  in optionals (vhost.ipv4 != null) (map (processNode vhost.ipv4) scfg) ++
     optionals (vhost.ipv6 != null) (map (processNode "[${vhost.ipv6}]") scfg);

  genSSLVHosts = vhost: configuration: genIPv46VHosts vhost [
    {
      type = "static";
      on = vhost.fqdn;
      socket = ":443";

      socketConfig = ''
        ssl.engine  = "enable"
        ssl.pemfile = "${vhost.ssl.privateKey.path}"
        ssl.ca-file = "${vhost.ssl.intermediateCert}"
      '';

      inherit configuration;
    }
    {
      socket = ":80";
      socketConfig = ''
        url.redirect = ( "^/(.*)" => "https://headcounter.org/$1" )
      '';
    }
  ];
in {
  imports = [ ../domains.nix ];

  deployment.hetzner.partitions = ''
    clearpart --all --initlabel --drives=sda,sdb

    part swap1 --recommended --label=swap1 --fstype=swap --ondisk=sda
    part swap2 --recommended --label=swap2 --fstype=swap --ondisk=sdb

    part btrfs.1 --grow --ondisk=sda
    part btrfs.2 --grow --ondisk=sdb

    btrfs / --data=1 --metadata=1 --label=root btrfs.1 btrfs.2
  '';

  boot.kernelPackages = pkgs.linuxPackages_latest;

  fileSystems."/".options = concatStringsSep "," [
    "autodefrag"
    "space_cache"
    "inode_cache"
    "compress=lzo"
    "noatime"
  ];

  services.headcounter.lighttpd = {
    enable = true;
    defaultPort = null;

    configuration = ''
      mimetype.assign = (
        ".html" => "text/html",
        ".js" => "text/javascript"
      )
    '';

    modules.proxy.enable = true;
    modules.magnet.enable = true;
    modules.setenv.enable = true;
    modules.redirect.enable = true;

    virtualHosts = with config.headcounter.vhosts; genSSLVHosts headcounter ''
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
          "host" => "${hydraIPv4}",
          "port" => 3000
        )))
      } # http://redmine.lighttpd.net/issues/1268
      else $HTTP["url"] =~ "" {
        server.document-root = "${mainSite.html}"
      }
    '';
  };

  users.extraGroups.telnetsite.gid = 497;
  users.extraUsers.telnetsite = {
    uid = 496;
    description = "Headcounter Telnet Site User";
    group = "telnetsite";
  };

  systemd.sockets."telnet-site" = {
    description = "Headcounter Telnet Socket";
    wantedBy = [ "sockets.target" ];
    before = [ "multi-user.target" ];
    socketConfig.ListenStream = 23;
    socketConfig.Accept = true;
  };

  systemd.services."telnet-site@" = {
    description = "Headcounter Telnet Site";
    serviceConfig = {
      User = "telnetsite";
      Group = "telnetsite";
      PrivateTmp = true;
      PrivateNetwork = true;
      ExecStart = "-${pkgs.telnet}/sbin/in.telnetd -h -L "
                + "${mainSite}/bin/headcounter";
      StandardInput = "socket";
      MemoryLimit = "10M";
      OOMScoreAdjust = 1000;
    };
  };
}
