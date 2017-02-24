{ pkgs, hclib, toplevelConfig, defaults, ... }:

{ lib, config, ... }:

with lib;

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
  mkModuleEx = { desc, deps ? [], xep ? null, odbc ? false }: name: {
    enable = mkOption rec {
      type = types.bool;
      default = attrByPath [ name "enable" ] false defaults;
      example = !default;
      description = ''
        Whether to enable the ${optionalString odbc "ODBC"} module for ${desc}.
      '' + optionalString (xep != null) (let
        padded = fixedWidthString 4 "0" (toString xep);
        url = "http://xmpp.org/extensions/xep-${padded}.html";
      in ''
        This module implements <link xlink:href="${url}">XEP-${padded}</link>.
      '');
    };

    options = mkOption {
      type = types.unspecified;
      default = attrByPath [ name "options" ] {} defaults;
      description = ''
        Options for the ${name} module which are directly converted to Erlang
        expressions from the given Nix expression.

        <table tocentry="0">
          <title>Nix to Erlang type mapping</title>
          <tgroup cols="4">
            <thead>
              <row>
                <entry>Nix type</entry>
                <entry>Erlang type</entry>
                <entry>Nix example</entry>
                <entry>Erlang result</entry>
              </row>
            </thead>
            <tbody>
              <row>
                <entry>int</entry>
                <entry>int</entry>
                <entry><literal>666</literal></entry>
                <entry><literal>666</literal></entry>
              </row>
              <row>
                <entry>string</entry>
                <entry>string</entry>
                <entry><literal>&quot;hello world&quot;</literal></entry>
                <entry><literal>&quot;hello world&quot;</literal></entry>
              </row>
              <row>
                <entry>boolean</entry>
                <entry>boolean (atom)</entry>
                <entry><literal>true</literal></entry>
                <entry><literal>true</literal></entry>
              </row>
              <row>
                <entry>list</entry>
                <entry>list</entry>
                <entry><literal>[ 1 2 3 ]</literal></entry>
                <entry><literal>[1, 2, 3]</literal></entry>
              </row>
              <row>
                <entry>attribute set</entry>
                <entry>property list</entry>
                <entry><literal>{ foo = 123; bar = 456; }</literal></entry>
                <entry><literal>[{foo, 123}, {bar, 456}]</literal></entry>
              </row>
              <row>
                <entry>attribute set consisting only of a
                       <literal>atom</literal> attribute name</entry>
                <entry>atom</entry>
                <entry><literal>{ atom = &quot;foo&quot;; }</literal></entry>
                <entry><literal>foo</literal></entry>
              </row>
              <row>
                <entry>attribute set consisting only of a
                       <literal>tuple</literal> attribute name</entry>
                <entry>tuple</entry>
                <entry><literal>{ tuple = [1 2 3]; }</literal></entry>
                <entry><literal>{1, 2, 3}</literal></entry>
              </row>
              <row>
                <entry>attribute set consisting only of a
                       <literal>binary</literal> attribute name</entry>
                <entry>binary</entry>
                <entry><literal>{ binary = &quot;foo&quot; }</literal></entry>
                <entry><literal>${
                  "&lt;&lt;&quot;foo&quot;&gt;&gt;"
                }</literal></entry>
              </row>
            </tbody>
          </tgroup>
        </table>
      '';
    };
  };

  mkModule = desc: xep: mkModuleEx { inherit desc xep; };
  mkModuleDeps = deps: desc: xep: mkModuleEx { inherit deps desc xep; };
  mkModuleDep = dep: mkModuleDeps (singleton dep);
  mkModuleODBC = desc: xep: mkModuleEx { inherit desc xep; odbc = true; };

  mkXepLink = number: let
    url = "http://xmpp.org/extensions/xep-${number}.html";
  in "<link xlink:href=\"${url}\">XEP-${number}</link>";

  modules = {
    adhoc = mkModule "Ad-Hoc Commands" 50;
    admin_extra = mkModule "Administrative functions and commands" null;
    amp = mkModule "Advanced Message Processing" 79;
    blocking = mkModule "Blocking Command" 191;
    bosh = mkModule "XMPP over Bosh service (HTTP Binding)" null;
    caps = mkModule "Request and cache Entity Capabilities" 115;
    carboncopy = mkModule "Carbon Copies" 280;
    csi = mkModule "Client State Indication" 352;
    commands = mkModule "General purpose commands" null;
    disco = mkModule "Service Discovery" 30;
    last = mkModuleODBC "Last Activity" 12;
    mam_meta = mkModule "Message Archive Management" 313;
    muc = mkModule "Multi-User Chat" 45;
    muc_commands = mkModule "Commands for Multi-User Chat" null;
    muc_light_commands = mkModule "Commands for MUC light" null;
    muc_log = mkModuleDep "mod_muc" "Multi-User Chat room logging" null;
    offline = mkModule "Offline message storage" 160;
    ping = mkModule "XMPP Ping and periodic keepalives" 199;
    privacy = mkModuleODBC "Blocking Communication" 16;
    private = mkModuleODBC "Private XML Storage" 49;
    pubsub = mkModule "Publish-Subscribe" 60;
    register = mkModule "In-Band Registration" 77;
    roster = mkModuleODBC "Roster management (XMPP IM)" null;
    sic = mkModule "Server IP Check" 279;
    stream_management = mkModule "Stream management" 198;
    vcard = mkModuleODBC "vcard-temp" 54;
  };

  modulesWithDefaults = mapAttrs (name: f: f name) modules;
in {
  options = modulesWithDefaults // {
    expression = lib.mkOption {
      type = lib.types.attrs;
      internal = true;
      description = ''
        The resulting Nix expression that's translated to an Erlang expression
        for the options given by this submodule.
      '';
    };
  };

  config.expression = let
    justModules = genAttrs (attrNames modules) (mod: config.${mod});
    enabled = filterAttrs (name: mod: mod.enable) justModules;
  in flip mapAttrs' enabled (name: cfg: {
    name = "mod_${name}";
    value = cfg.options;
  });
}
