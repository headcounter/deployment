{ pkgs, lib, ... }:

let
  softhsm = pkgs.stdenv.mkDerivation rec {
    name = "softhsm-${version}";
    version = "1.3.8";

    src = pkgs.fetchurl {
      url = "https://dist.opendnssec.org/source/${name}.tar.gz";
      sha256 = "0flmnpkgp65ym7w3qyg78d3fbmvq3aznmi66rgd420n33shf7aif";
    };

    configureFlags = [ "--with-botan=${pkgs.botan}" ];
    buildInputs = [ pkgs.sqlite ];
  };

  pkcs11-proxy = pkgs.stdenv.mkDerivation {
    name = "pkcs11-proxy";

    src = pkgs.fetchFromGitHub {
      owner = "SUNET";
      repo = "pkcs11-proxy";
      rev = "944684f78bca0c8da6cabe3fa273fed3db44a890";
      sha256 = "1nxgd29y9wmifm11pjcdpd2y293p0dgi0x5ycis55miy97n0f5zy";
    };

    postPatch = "patchShebangs mksyscalls.sh";

    nativeBuildInputs = [ pkgs.cmake ];
    buildInputs = [ pkgs.openssl pkgs.libseccomp ];
  };

  goose = let
    owner = "liamstask";
    repo = "goose";
    rev = "8488cc47d90c8a502b1c41a462a6d9cc8ee0a895";
    version = "20150116";

  in pkgs.buildGoPackage rec {
    name = "${repo}-${version}";

    src = pkgs.fetchFromBitbucket {
      name = "${name}-src";
      inherit rev owner repo;
      sha256 = "1jy0pscxjnxjdg3hj111w21g8079rq9ah2ix5ycxxhbbi3f0wdhs";
    };

    goPackagePath = "bitbucket.org/${owner}/${repo}";
    subPackages = [ "cmd/goose" ];
    extraSrcs = let
      mkDep = { goPackagePath, url ? goPackagePath, rev, sha256 }: {
        inherit goPackagePath;
        src = pkgs.fetchgit { url = "https://${url}"; inherit rev sha256; };
      };
    in map mkDep [
      { goPackagePath = "github.com/go-sql-driver/mysql";
        rev = "2e00b5cd70399450106cec6431c2e2ce3cae5034";
        sha256 = "085g48jq9hzmlcxg122n0c4pi41sc1nn2qpx1vrl2jfa8crsppa5";
      }
      { goPackagePath = "github.com/kylelemons/go-gypsy";
        rev = "08cad365cd28a7fba23bb1e57aa43c5e18ad8bb8";
        sha256 = "1djv7nii3hy451n5jlslk0dblqzb1hia1cbqpdwhnps1g8hqjy8q";
      }
      { goPackagePath = "github.com/lib/pq";
        rev = "ba5d4f7a35561e22fbdf7a39aa0070f4d460cfc0";
        sha256 = "1mfbqw9g00bk24bfmf53wri5c2wqmgl0qh4sh1qv2da13a7cwwg3";
      }
      { goPackagePath = "github.com/mattn/go-sqlite3";
        rev = "2acfafad5870400156f6fceb12852c281cbba4d5";
        sha256 = "1rpgil3w4hh1cibidskv1js898hwz83ps06gh0hm3mym7ki8d5h7";
      }
      { goPackagePath = "github.com/ziutek/mymysql";
        rev = "0582bcf675f52c0c2045c027fd135bd726048f45";
        sha256 = "0bkc9x8sgqbzgdimsmsnhb0qrzlzfv33fgajmmjxl4hcb21qz3rf";
      }
      { goPackagePath = "golang.org/x/net";
        url = "go.googlesource.com/net";
        rev = "10c134ea0df15f7e34d789338c7a2d76cc7a3ab9";
        sha256 = "14cbr2shl08gyg85n5gj7nbjhrhhgrd52h073qd14j97qcxsakcz";
      }
    ];
  };

  boulder = let
    owner = "letsencrypt";
    repo = "boulder";
    rev = "hotfixes/2017-02-01";
    version = "20170201";

  in pkgs.buildGoPackage rec {
    name = "${repo}-${version}";

    src = pkgs.fetchFromGitHub {
      name = "${name}-src";
      inherit rev owner repo;
      sha256 = "01nvdizq38jzcmixshxlfzgdsmd6vimqnp3fi9agx2s1sdkgzfvh";
    };

    postPatch = ''
      find test -type f -exec sed -i -e '/libpkcs11-proxy.so/ {
        s,/usr/local,${pkcs11-proxy},
      }' {} +

      sed -i -r \
        -e '/^def +install/a \    return True' \
        -e 's,exec \./bin/,,' \
        test/startservers.py
    '';

    goPackagePath = "github.com/${owner}/${repo}";
    buildInputs = [ pkgs.libtool ];
  };

  boulderSource = "${boulder.out}/share/go/src/${boulder.goPackagePath}";

  softHsmConf = pkgs.writeText "softhsm.conf" ''
    0:/var/lib/softhsm/slot0.db
    1:/var/lib/softhsm/slot1.db
  '';

  cfgDir = "${boulderSource}/test/config";

  components = {
    gsb-test-srv.args = "-apikey my-voice-is-my-passport";
    gsb-test-srv.waitForPort = 6000;
    gsb-test-srv.first = true;
    boulder-sa.args = "--config ${cfgDir}/sa.json";
    boulder-wfe.args = "--config ${cfgDir}/wfe.json";
    boulder-ra.args = "--config ${cfgDir}/ra.json";
    boulder-ca.args = "--config ${cfgDir}/ca.json";
    boulder-va.args = "--config ${cfgDir}/va.json";
    boulder-publisher.args = "--config ${cfgDir}/publisher.json";
    boulder-publisher.waitForPort = 9091;
    ocsp-updater.args = "--config ${cfgDir}/ocsp-updater.json";
    ocsp-updater.after = [ "boulder-publisher" ];
    ocsp-responder.args = "--config ${cfgDir}/ocsp-responder.json";
    ct-test-srv = {};
    dns-test-srv = {};
    mail-test-srv.args = "--closeFirst 5";
  };

  commonPath = [ softhsm pkgs.mariadb goose boulder ];

  mkServices = a: b: with lib; listToAttrs (concatLists (mapAttrsToList a b));

  componentServices = mkServices (name: attrs: let
    mkSrvName = n: "boulder-${n}.service";
    firsts = lib.filterAttrs (lib.const (c: c.first or false)) components;
    firstServices = map mkSrvName (lib.attrNames firsts);
    firstServicesNoSelf = lib.remove "boulder-${name}.service" firstServices;
    additionalAfter = firstServicesNoSelf ++ map mkSrvName (attrs.after or []);
    needsPort = attrs ? waitForPort;
    inits = map (n: "boulder-init-${n}.service") [
      "rabbitmq" "mysql" "softhsm"
    ];
    portWaiter = {
      name = "boulder-${name}";
      value = {
        description = "Wait For Port ${toString attrs.waitForPort} (${name})";
        after = [ "boulder-real-${name}.service" ];
        requires = [ "boulder-real-${name}.service" ];
        requiredBy = [ "boulder.service" ];
        serviceConfig.Type = "oneshot";
        serviceConfig.RemainAfterExit = true;
        script = let
          netcat = "${pkgs.netcat-openbsd}/bin/nc";
          portCheck = "${netcat} -z 127.0.0.1 ${toString attrs.waitForPort}";
        in "while ! ${portCheck}; do :; done";
      };
    };
  in lib.optional needsPort portWaiter ++ lib.singleton {
    name = if needsPort then "boulder-real-${name}" else "boulder-${name}";
    value = {
      description = "Boulder ACME Component (${name})";
      after = inits ++ additionalAfter;
      requires = inits;
      requiredBy = [ "boulder.service" ];
      path = commonPath;
      environment.GORACE = "halt_on_error=1";
      environment.SOFTHSM_CONF = softHsmConf;
      environment.PKCS11_PROXY_SOCKET = "tcp://127.0.0.1:5657";
      serviceConfig.WorkingDirectory = boulderSource;
      serviceConfig.ExecStart = "${boulder}/bin/${name} ${attrs.args or ""}";
    };
  }) components;

in {
  networking.extraHosts = "127.0.0.1 ${toString [
    "sa.boulder" "ra.boulder" "wfe.boulder" "ca.boulder" "va.boulder"
    "publisher.boulder" "ocsp-updater.boulder" "admin-revoker.boulder"
    "boulder" "boulder-mysql" "boulder-rabbitmq"
  ]}";

  services.mysql.enable = true;
  services.mysql.package = pkgs.mariadb;

  services.rabbitmq.enable = true;
  services.rabbitmq.port = 5673;

  systemd.services = {
    pkcs11-daemon = {
      description = "PKCS11 Daemon";
      after = [ "boulder-init-softhsm.service" ];
      before = map (n: "${n}.service") (lib.attrNames componentServices);
      wantedBy = [ "multi-user.target" ];
      environment.SOFTHSM_CONF = softHsmConf;
      environment.PKCS11_DAEMON_SOCKET = "tcp://127.0.0.1:5657";
      serviceConfig.ExecStart = let
        softhsmLib = "${softhsm}/lib/softhsm/libsofthsm.so";
      in "${pkcs11-proxy}/bin/pkcs11-daemon ${softhsmLib}";
    };

    boulder-init-rabbitmq = {
      description = "Boulder ACME Init (RabbitMQ)";
      after = [ "rabbitmq.service" ];
      serviceConfig.Type = "oneshot";
      serviceConfig.RemainAfterExit = true;
      serviceConfig.WorkingDirectory = boulderSource;
      path = commonPath;
      script = "rabbitmq-setup -server amqp://localhost:5673";
    };

    boulder-init-mysql = {
      description = "Boulder ACME Init (MySQL)";
      after = [ "mysql.service" ];
      serviceConfig.Type = "oneshot";
      serviceConfig.RemainAfterExit = true;
      serviceConfig.WorkingDirectory = boulderSource;
      path = commonPath;
      script = "${pkgs.bash}/bin/sh test/create_db.sh";
    };

    boulder-init-softhsm = {
      description = "Boulder ACME Init (SoftHSM";
      environment.SOFTHSM_CONF = softHsmConf;
      serviceConfig.Type = "oneshot";
      serviceConfig.RemainAfterExit = true;
      serviceConfig.WorkingDirectory = boulderSource;
      preStart = "mkdir -p /var/lib/softhsm";
      path = commonPath;
      script = ''
        softhsm --slot 0 --init-token \
          --label intermediate --pin 5678 --so-pin 1234
        softhsm --slot 0 --import test/test-ca.key \
          --label intermediate_key --pin 5678 --id FB
        softhsm --slot 1 --init-token \
          --label root --pin 5678 --so-pin 1234
        softhsm --slot 1 --import test/test-root.key \
          --label root_key --pin 5678 --id FA
      '';
    };

    boulder = {
      description = "Boulder ACME Server";
      after = map (n: "${n}.service") (lib.attrNames componentServices);
      wantedBy = [ "multi-user.target" ];
      serviceConfig.Type = "oneshot";
      serviceConfig.RemainAfterExit = true;
      script = let
        ports = lib.range 8000 8005 ++ lib.singleton 4000;
        netcat = "${pkgs.netcat-openbsd}/bin/nc";
        mkPortCheck = port: "${netcat} -z 127.0.0.1 ${toString port}";
        portCheck = "(${lib.concatMapStringsSep " && " mkPortCheck ports})";
      in "while ! ${portCheck}; do :; done";
    };
  } // componentServices;
}
