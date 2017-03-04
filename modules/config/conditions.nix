{ pkgs, config, lib, hclib, ... }:

let
  inherit (lib) types mkOption;

  conditionMod = { name, ... }: {
    options.description = mkOption {
      type = types.str;
      default = "Startup Conditions for ${name}";
      description = ''
        The description for the condition unit that's started before the actual
        service.
      '';
    };

    options.connectable = {
      address = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          An IPv4 address or hostname to check for availability.

          If the value is <literal>null</literal>, the local machine is
          assumed.
        '';
      };

      port = mkOption {
        type = types.nullOr hclib.types.port;
        default = null;
        description = ''
          The TCP port to check for availability.
        '';
      };
    };

    options.bindable = {
      address = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          Check whether the specified IPv4 address can be bound to a socket.
        '';
      };
    };
  };

  checkInet = pkgs.headcounter.compileC {
    name = "check-inet";
    source = ''
      #include <netdb.h>
      #include <stdio.h>
      #include <stdlib.h>
      #include <unistd.h>
      #include <string.h>
      #include <strings.h>
      #include <sys/socket.h>
      #include <arpa/inet.h>

      int check_port(int sockfd, const char *host, int port) {
        struct hostent *target;
        struct sockaddr_in addr;

        if ((target = gethostbyname(host)) == NULL)
          return -1;

        bzero((char*)&addr, sizeof(addr));
        addr.sin_family = AF_INET;
        addr.sin_port = htons(port);
        bcopy((char *)target->h_addr, (char *)&addr.sin_addr.s_addr,
              target->h_length);

        return connect(sockfd, (struct sockaddr*)&addr, sizeof(addr));
      }

      int check_bind(int sockfd, const char *bindaddr) {
        struct sockaddr_in addr;

        bzero((char*)&addr, sizeof(addr));
        addr.sin_family = AF_INET;
        addr.sin_port = 0;
        addr.sin_addr.s_addr = inet_addr(bindaddr);
        return bind(sockfd, (struct sockaddr *)&addr, sizeof(addr));
      }

      int print_usage(const char *prog) {
        fprintf(stderr, "Usage: %s -port [address] PORT\n", prog);
        fprintf(stderr, "       %s -bind ADDRESS\n",        prog);
        return EXIT_FAILURE;
      }

      #define LOOP(cmd, ...) \
        if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) { \
          perror("socket"); \
          return EXIT_FAILURE; \
        } \
        while (cmd(sockfd, __VA_ARGS__) != 0) sleep(1); \
        close(sockfd)

      int main(int argc, char *argv[]) {
        int sockfd;

        if (argc <= 2)
          return print_usage(argv[0]);

        if (!strcmp(argv[1], "-port") != 0) {
          switch (argc) {
            case 3: LOOP(check_port, "127.0.0.1", atoi(argv[2])); break;
            case 4: LOOP(check_port, argv[2],     atoi(argv[3])); break;
            default: return print_usage(argv[0]);
          }
        } else if (!strcmp(argv[1], "-bind") != 0 && argc == 3) {
          LOOP(check_bind, argv[2]);
        } else {
          return print_usage(argv[0]);
        }

        return EXIT_SUCCESS;
      }
    '';
  };

  mkService = prefix: name: cfg: command: {
    name = "condition-${prefix}-${name}.service";
    value = {
      inherit (cfg) description;
      requiredBy = [ "${name}.service" ];
      before = [ "${name}.service" ];
      after = [ "network.target" ];
      serviceConfig = {
        ExecStart = let
          mkArg = arg: lib.escapeShellArg (toString arg);
        in lib.concatMapStringsSep " " mkArg command;
        Type = "oneshot";
        RemainAfterExit = true;
      };
    };
  };

  mkServices = specFun: defs: let
    getRelevant = cfg: lib.filter (def: def.condition != null) (specFun cfg);
    mapSpec = name: cfg: attrs: mkService attrs.prefix name cfg attrs.command;
    mapDef = name: cfg: map (mapSpec name cfg) (getRelevant cfg);
  in lib.listToAttrs (lib.concatLists (lib.mapAttrsToList mapDef defs));

in {
  options.headcounter.conditions = mkOption {
    type = types.attrsOf (types.submodule conditionMod);
    default = {};
    description = ''
      Conditions that need to fulfilled before starting the service denoted by
      the attribute name.
    '';
  };

  config = lib.mkIf (config.headcounter.conditions != {}) {
    systemd.services = mkServices (cfg: [
      { condition = cfg.connectable.port;
        prefix = "port";
        command = let
          inherit (cfg.connectable) address;
          maybeAddr = lib.optional (address != null) address;
        in [ checkInet "-port" ] ++ maybeAddr ++ [ cfg.connectable.port ];
      }
      { condition = cfg.bindable.address;
        prefix = "bind";
        command = [ checkInet "-bind" cfg.bindable.address ];
      }
    ]) config.headcounter.conditions;
  };
}
