lib:

{
  enumDoc = attrs: ''
    <variablelist>
      ${lib.concatStrings (lib.flip lib.mapAttrsToList attrs (option: doc: ''
        <varlistentry>
          <term><option>${option}</option></term>
          <listitem><para>${doc}</para></listitem>
        </varlistentry>
      ''))}
    </variablelist>
  '';

  # An option for specifying multiple listeners for use in socket activated
  # services.
  mkListenerOption = desc: defHost: defPort: lib.mkOption {
    type = lib.types.listOf (lib.types.submodule {
      options.host = lib.mkOption {
        type = lib.types.str;
        example = "::";
        description = ''
          Hostname, IPv4 or IPv6 address to listen for ${desc}.
        '';
      };

      options.port = lib.mkOption {
        type = (import ./types.nix lib).types.port;
        default = defPort;
        description = "Port to listen for ${desc}.";
      };

      options.device = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "tun1000";
        description = "The network device to bind to for ${desc}.";
      };
    });

    default = lib.singleton { host = defHost; };
    example = [
      { host = "localhost"; }
      { host = "1.2.3.4"; port = 666; device = "eth0"; }
    ];

    description = "Hosts/Ports/Devices to listen for ${desc}.";
  };

  # The counterpart to 'mkListenerOption', which creates the actual socket
  # units.
  #
  # For example:
  #
  #   systemd.sockets = hclib.mkSocketConfig {
  #     namePrefix = "my-shiny-daemon";
  #     description = "Socket For My Shiny Daemon";
  #     config = config.services.my-shiny-daemon.listen;
  #   };
  #
  mkSocketConfig =
    # The name prefix for the socket unit, which will be suffixed with a
    # number. For example if the namePrefix is "foo", the first socket unit
    # will be called "foo-1.socket".
    { namePrefix
    # A description for the socket unit.
    , description
    # An optional file descriptor name used for $LISTEN_FDNAMES.
    # See sd_listen_fds_with_names(3) for more information.
    , fdName ? null
    # The unit name (without .service suffix) of the service requiring the
    # sockets.
    , service ? namePrefix
    # The option definitions of the option declaration made with
    # mkListenerOption.
    , config
    # Extra attributes to add to the socketConfig attribute of the sockets.
    , extraSocketConfig ? {}
    }:

    builtins.listToAttrs (lib.imap (number: lcfg: {
      name = "${namePrefix}-${toString number}";
      value = let
        isV6 = builtins.match ".*:.*" lcfg.host != null;
        host = if isV6 then "[${lcfg.host}]" else lcfg.host;
        hostPort = "${host}:${toString lcfg.port}";
      in {
        description = "${description} (${hostPort})";
        wantedBy = let
          devUnit = "sys-subsystem-net-devices-${lcfg.device}.device";
          target = if lcfg.device == null then "sockets.target" else devUnit;
        in lib.singleton target;
        requiredBy = lib.singleton "${service}.service";
        socketConfig = {
          # FreeBind is needed, because even while the device is up, an IP
          # address might not have been assigned yet.
          FreeBind = true;
          Service = "${service}.service";
          ListenStream = hostPort;
        } // lib.optionalAttrs (lcfg.device != null) {
          BindToDevice = lcfg.device;
        } // lib.optionalAttrs (fdName != null) {
          FileDescriptorName = fdName;
        } // extraSocketConfig;
      };
    }) config);
}
