with import <nixpkgs> {};

let
  buildInputs = [ pkgs.openssl ];

  rootCA = stdenv.mkDerivation {
    name = "snakeoil-ca-root";
    inherit buildInputs;
    buildCommand = ''
      ensureDir "$out"
      openssl genrsa -out "$out/private.key" 2048
      openssl req -x509 -new -nodes -key "$out/private.key" -days 40000 \
        -extensions v3_ca -out "$out/root.pem" <<INPUT
      DE
      Snakeoil root CA
      Snakeoil root CA
      Snakeoil root CA
      Snakeoil root CA
      Snakeoil root CA
      snakeoil-root@example.org
      INPUT
    '';
  };

  intermediate = stdenv.mkDerivation {
    name = "snakeoil-ca-intermediate";
    inherit buildInputs;
    buildCommand = ''
      ensureDir "$out"
      openssl genrsa -out "$out/private.key" 2048
      openssl req -new -nodes -key "$out/private.key" -days 40000 \
        -extensions v3_ca -out intermediate.csr <<INPUT
      DE
      Snakeoil intermediate CA
      Snakeoil intermediate CA
      Snakeoil intermediate CA
      Snakeoil intermediate CA
      Snakeoil intermediate CA
      snakeoil-intermediate@example.org


      INPUT

      openssl x509 -extensions v3_ca \
        -req -in intermediate.csr \
        -CA "${rootCA}/root.pem" \
        -CAkey "${rootCA}/private.key" \
        -set_serial 666 \
        -extensions v3_ca \
        -out "$out/intermediate.pem"
    '';
  };

  mkCert = cn: stdenv.mkDerivation {
    name = "snakeoil-${cn}";
    inherit buildInputs;
    buildCommand = ''
      ensureDir "$out"
      openssl genrsa -out "$out/private.key" 2048
      openssl req -new -nodes -key "$out/private.key" -out crt.req <<INPUT
      DE
      Snakeoil
      Snakeoil
      Snakeoil
      Snakeoil
      ${cn}
      snakeoil@example.org


      INPUT

      openssl x509 -extensions v3_ca \
        -req -in crt.req \
        -CA "${intermediate}/intermediate.pem" \
        -CAkey "${intermediate}/private.key" \
        -set_serial "0x${builtins.hashString "md5" cn}" \
        -days 40000 \
        -out "$out/public.pem"

      if ! openssl verify -CAfile "${rootCA}/root.pem" "$out/public.pem"; then
        cat "${intermediate}/intermediate.pem" "$out/public.pem" | \
          openssl verify -CAfile "${rootCA}/root.pem"
      else
        echo "$out/public.pem verified successfully without using" >&2
        echo "intermediate certificate. This should not happen!" >&2
        exit 1
      fi
    '';
  };
in cn: let
  cert = mkCert cn;
in {
  privateKey = "${cert}/private.key";
  publicKey = "${cert}/public.pem";
  intermediateCert = "${intermediate}/intermediate.pem";
  rootCert = "${rootCA}/root.pem";
}
