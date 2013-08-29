{ config, pkgs, ... }:

with pkgs.lib;

let
  cfg = config.services.headcounter.lighttpd;
  lighttpd = pkgs.lighttpd;
  lightyCfgFile = pkgs.writeText "lighttpd.conf" lightyConf;

  lightyEscape = value: "\"${escape ["\""] value}\"";

  toLightyList = items: concatStringsSep ", " (map lightyEscape items);

  lightyConf = ''
    server.document-root = ${lightyEscape cfg.defaultDocroot}
    server.port = ${toString cfg.defaultPort}
    server.modules = (${toLightyList (map (x: "mod_" + x) enabledModules)})
    server.username = ${lightyEscape cfg.user}
    server.groupname = ${lightyEscape cfg.group}
    server.errorlog = ${lightyEscape (cfg.logDir + "/errors.log")}
    server.upload-dirs = (${toLightyList cfg.uploadDirs})
    index-file.names = (${toLightyList cfg.indexFileNames})
    ${concatStringsSep "\n" modulesConfig}
    ${mkVirtualHosts cfg.virtualHosts}
    ${cfg.configuration}
  '';

  mkVirtualHosts = vhosts: let
    genVHost = h: let
      op =
        if h.type == "static" then "=="
        else if h.type == "!static" then "!="
        else if h.type == "regex" then "=~"
        else if h.type == "!regex" then "!~"
        else throw "Unknown virtual host match type ${h.type}!";
    in
      ''
        $HTTP["host"] ${op} ${lightyEscape h.on} {
          ${optionalString (h ? docroot) ''
          server.document-root = ${lightyEscape h.docroot}
          ''}
          ${if h ? configuration then h.configuration else ""}
        }
      '';
  in concatStringsSep "else " (map genVHost vhosts);

  getCfgList = check: getter: attrs:
    attrValues (mapAttrs getter (filterAttrs (_: check) attrs));

  getFromEnabledModules = getter: getCfgList (x: x.enable) getter cfg.modules;

  enabledModules = getFromEnabledModules (k: _: k);
  modulesConfig = getFromEnabledModules (_: x: x.config);

  systemUsers =
    getCfgList (x: x.isSystemUser) (_: x: x.name) config.users.extraUsers;

  createDir = dir: ''
    if ! test -e "${dir}"; then
      install -v -m0770 -o "${cfg.user}" -g "${cfg.group}" -d "${dir}";
    fi;
  '';

in {
  options.services.headcounter.lighttpd = let
    mkOptModuleFull = desc: ex: def: {
      enable = mkOption {
        default = false;
        description = "Enable the ${desc} module.";
      };

      config = mkOption {
        description = "Default configuration for ${desc} module.";
        default = "";
      } // optionalAttrs (ex != null) {
        example = ex;
      } // optionalAttrs (def != null) {
        default = def;
      };
    };
    mkOptModule = desc: mkOptModuleFull desc null null;
    mkOptModuleDef = desc: def: mkOptModuleFull desc null def;
    mkOptModuleEx = desc: ex: mkOptModuleFull desc ex null;
  in {
    enable = mkOption {
      default = false;
      description = "Enable the lighttpd web server.";
    };

    user = mkOption {
      default = "lighttpd";
      description = "User under which lighttpd runs.";
    };

    group = mkOption {
      default = "lighttpd";
      description = "Group under which lighttpd runs.";
    };

    defaultPort = mkOption {
      default = 80;
      description = "Default port where lighttpd should listen on.";
    };

    virtualHosts = mkOption {
      default = [];
      description = "Virtual host definitions.";
      example = [
        {
          type = "static";
          on = "www.example.com";
          docroot = "/path/to/docroot";
        }
        {
          type = "regex";
          on = "^www\.example\.(?:net|org)\$";
          docroot = "/path/to/another/docroot";
        }
      ];
    };

    configuration = mkOption {
      default = "";
      description = "Additional lighttpd configuration.";
      example = ''
        server.max-keep-alive-requests = 4
        server.max-keep-alive-idle = 4
      '';
    };

    defaultDocroot = mkOption {
      default = pkgs.runCommand "empty" {} "ensureDir $out";
      description = "Default document root.";
    };

    indexFileNames = mkOption {
      default = [ "index.html" "index.htm" "default.htm" ];
      description = "List of files to search for if a directory is requested.";
    };

    logDir = mkOption {
      default = "/var/log/lighttpd";
      description = "Directory where lighttpd will place log files";
    };

    uploadDirs = mkOption {
      default = [ "/var/cache/lighttpd/uploads" ];
      description = "Directories where uploaded files will be stored.";
    };

    compressCacheDir = mkOption {
      default = "/var/cache/lighttpd/compress";
      description = "Directories where compressed files are cached.";
    };

    modules = {
      access = mkOptModuleDef "access restrictions" ''
        url.access-deny = ("~", ".inc")
      '';
      accesslog = mkOptModuleDef "access log" ''
        accesslog.filename = "${cfg.logDir}/access.log"
      '';
      alias = mkOptModule "directory aliases";
      auth = mkOptModule "authentication";
      cgi = mkOptModuleDef "CGI" ''
        $HTTP["url"] =~ "^/cgi-bin/" {
          cgi.assign = ( "" => "" )
        }
      '';
      compress = mkOptModuleDef "output compression" ''
        compress.cache-dir = "${cfg.compressCacheDir}";
        compress.filetype = (
          "application/x-javascript",
          "text/css",
          "text/html",
          "text/plain"
        )
      '';
      dirlisting = mkOptModuleDef "directory listing" ''
        dir-listing.activate = "enable"
        dir-listing.encoding = "utf-8"
      '';
      evasive = mkOptModule "evasive";
      evhost = mkOptModule "enhanced virtual hosting";
      expire = mkOptModule "cached expiration";
      extforward = mkOptModuleEx "X-Forwarded-For header extraction" ''
        extforward.forwarder = (
          "10.0.0.232" => "trust",
          "10.0.0.233" => "trust"
        )
      '';
      fastcgi = mkOptModule "FastCGI";
      flv_streaming = mkOptModule "FLV streaming";
      mysql_vhost = mkOptModuleEx "MySQL virtual hosting" ''
        mysql-vhost.db       = "lighttpd"
        mysql-vhost.user     = "lighttpd"
        mysql-vhost.pass     = "secret"
        mysql-vhost.sock     = "/var/run/mysqld/mysqld.sock"
        mysql-vhost.sql      = "SELECT docroot FROM domains WHERE domain='?';"
        mysql-vhost.hostname = "localhost"
        mysql-vhost.port     = 3306
      '';
      proxy = mkOptModuleEx "proxy" ''
        proxy.server = (
          ".jsp" => (
            ( "host" => "10.0.0.242", "port" => 81 )
          )
        )
      '';
      redirect = mkOptModuleEx "URL redirection" ''
        $HTTP["host"] =~ "^([^.]+\.[^.]+)$" {
          url.redirect = (
            ".*" => "http://www.%1"
          )
        }
      '';
      rewrite = mkOptModuleEx "URL rewrites" ''
        url.rewrite-once = (
          "^/foo"  => "$0",
          "^/(.*)" => "/handler/$1"
        )
      '';
      rrdtool = mkOptModuleDef "RRDtool" ''
        rrdtool.binary = "${pkgs.rrdtool}/bin/rrdtool"
        rrdtool.db-name = "${cfg.logDir}/lighttpd.rrd"
      '';
      scgi = mkOptModule "SCGI";
      secure_download = mkOptModule "secure and fast downloading";
      setenv = mkOptModuleEx "conditional request headers" ''
        setenv.add-environment = (
          "TRAC_ENV" => "lighttpd",
          "RAILS_ENV" => "production"
        )
      '';
      simple_vhost = mkOptModuleEx "simple virtual hosting" ''
        simple-vhost.server-root = "/var/www/servers/"
        simple-vhost.default-host = "example.org"
        simple-vhost.document-root = "pages"
      '';
      ssi = mkOptModuleDef "server side includes" ''
        ssi.extension = ".shtml"
      '';
      status = mkOptModuleEx "server status" ''
        status.status-url = "/server-status"
        status.config-url = "/server-config"
        status.statistics-url = "/server-statistics"
      '';
      trigger_b4_dl = mkOptModuleEx "trigger before download" ''
        trigger-before-download.gdbm-filename = "/srv/trigger.db"
        trigger-before-download.memcache-hosts = ( "127.0.0.1:11211" )
        trigger-before-download.trigger-url = "^/trigger/"
        trigger-before-download.download-url = "^/download/"
        trigger-before-download.deny-url = "http://10.0.0.242:1025/index.html"
        trigger-before-download.trigger-timeout = 10
      '';
      userdir = mkOptModuleDef "user directories" ''
        userdir.path = "public_html"
        userdir.exclude-user = (${toLightyList systemUsers})
      '';
      usertrack = mkOptModule "user tracking";
      webdav = mkOptModuleEx "WebDAV" ''
        $HTTP["url"] =~ "^/dav(?:$|/)" {
          webdav.activate = "enable"
          webdav.is-readonly = "enable"
          webdav.sqlite-db-name = "/srv/lighttpd.webdav_lock.db"
        }
      '';
    };
  };

  config = mkIf config.services.headcounter.lighttpd.enable {
    users.extraUsers = singleton {
      name = cfg.user;
      description = "Lighttpd user";
    };

    users.extraGroups = singleton {
      name = cfg.group;
    };

    systemd.services.lighttpd = {
      description = "Lighttpd webserver";
      wantedBy = [ "multi-user.target" ];
      requires = [ "keys.target" ];
      after = [ "network.target" "fs.target" "keys.target" ];

      preStart = ''
        ${concatMapStrings createDir cfg.uploadDirs}
        ${createDir cfg.compressCacheDir}
        ${createDir cfg.logDir}
      '';

      serviceConfig.PrivateTmp = true;
      serviceConfig.ExecStart = "${lighttpd}/sbin/lighttpd "
                              + "-D -f ${lightyCfgFile}";
      serviceConfig.ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
      serviceConfig.KillSignal = "SIGINT";
      serviceConfig.Restart = "always";
    };
  };
}
