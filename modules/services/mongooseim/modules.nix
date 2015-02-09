{ pkgs, toplevelConfig, defaults, ... }:

{ lib, config, ... }:

with lib;
with import ./erlexpr.nix;

/*
  Missing modules from Ejabberd:

  mod_announce            Manage announcements
                          DEPENDS: recommends mod_adhoc
  mod_blocking            Simple Communications Blocking (XEP-0191)
                          DEPENDS: mod_privacy
  mod_caps                Entity Capabilities (XEP-0115)
  mod_configure           Server configuration using Ad-Hoc
                          DEPENDS: mod_adhoc
  mod_echo                Echoes XMPP stanzas
  mod_http_fileserver     Small HTTP file server
  mod_irc                 IRC transport
  mod_offline_odbc        Offline message storage (XEP-0160)
                          DEPENDS: ODBC...
  mod_proxy65             SOCKS5 Bytestreams (XEP-0065)
  mod_pubsub              Pub-Sub (XEP-0060), PEP (XEP-0163)
                          DEPENDS: mod_caps
  mod_pubsub_odbc         Pub-Sub (XEP-0060), PEP (XEP-0163)
                          DEPENDS: supported DB (*) and mod_caps
  mod_register_web        Web for Account Registrations
  mod_service_log         Copy user messages to logger service
  mod_shared_roster       Shared roster management
                          DEPENDS: mod_roster or mod_roster_odbc
  mod_shared_roster_ldap  LDAP Shared roster management
                          DEPENDS: mod_roster or mod_roster_odbc
  mod_stats               Statistics Gathering (XEP-0039)
  mod_time                Entity Time (XEP-0202)
  mod_vcard_ldap          vcard-temp (XEP-0054)
                          DEPENDS: LDAP server
  mod_vcard_xupdate       vCard-Based Avatars (XEP-0153)
                          DEPENDS: mod_vcard or mod_vcard_odbc
  mod_version             Software Version (XEP-0092)
*/

let
  mkModuleEx = { desc, deps ? [], odbc ? false }: name: {
    enable = mkOption rec {
      type = types.bool;
      default = attrByPath [ name "enable" ] false defaults;
      example = !default;
      description = "Whether to enable the module for ${desc}.";
    };

    options = mkOption {
      type = types.unspecified;
      default = attrByPath [ name "options" ] {} defaults;
      description = ''
        Options for module ... TODO
      '';
    };
  };

  mkModule = desc: mkModuleEx { inherit desc; };
  mkModuleDeps = deps: desc: mkModuleEx { inherit deps desc; };
  mkModuleDep = dep: mkModuleDeps (singleton dep);
  mkModuleODBC = desc: mkModuleEx { inherit desc; odbc = true; };

  xep = number: let
    url = "http://xmpp.org/extensions/xep-${number}.html";
  in "<link xlink:href=\"${url}\">XEP-${number}</link>";

  modules = {
    adhoc = mkModule "Ad-Hoc Commands (${xep "0050"})";
    admin_extra = mkModule "Administrative functions and commands";
    amp = mkModule "Advanced Message Processing (${xep "0079"})";
    bosh = mkModule "XMPP over Bosh service (HTTP Binding)";
    carboncopy = mkModule "Carbon Copies (${xep "0280"})";
    disco = mkModule "Service Discovery (${xep "0030"})";
    last = mkModuleODBC "Last Activity (${xep "0012"})";
    mam = mkModule "Message Archive Management (${xep "0313"})";
    metrics = mkModule "MongooseIM metrics";
    muc = mkModule "Multi-User Chat (${xep "0045"})";
    muc_log = mkModuleDep "mod_muc" "Multi-User Chat room logging";
    offline = mkModule "Offline message storage (${xep "0160"})";
    ping = mkModule "XMPP Ping and periodic keepalives (${xep "0199"})";
    privacy = mkModuleODBC "Blocking Communication (${xep "0016"})";
    private = mkModuleODBC "Private XML Storage (${xep "0049"})";
    register = mkModule "In-Band Registration (${xep "0077"})";
    roster = mkModuleODBC "Roster management (XMPP IM)";
    sic = mkModule "Server IP Check (${xep "0279"})";
    snmp = mkModule "SNMP support";
    stream_management = mkModule "Stream management (${xep "0198"})";
    vcard = mkModuleODBC "vcard-temp (${xep "0054"})";
    websockets = mkModule "Websocket support";
  };

  modulesWithDefaults = mapAttrs (name: f: f name) modules;
in {
  options = modulesWithDefaults // {
    generatedConfig = mkOption {
      type = types.lines;
      default = "";
      internal = true;
      description = "Generated configuration values";
    };
  };

  config.generatedConfig = let
    justModules = removeAttrs config [ "generatedConfig" ];
    enabled = filterAttrs (name: mod: mod.enable) justModules;
    mkMod = name: cfg: "{mod_${name}, ${erlPropList cfg.options}}";
  in concatStringsSep ",\n  " (mapAttrsToList mkMod enabled);
}
