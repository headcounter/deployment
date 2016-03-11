{ stdenv, coreutils, gnutar, fetchFromBitbucket, makeWrapper, php }:

stdenv.mkDerivation {
  name = "xmppoke-genreport";

  src = fetchFromBitbucket {
    owner = "xnyhps";
    repo = "xmppoke-frontend";
    rev = "64ae8f9f8afe693b452042cafb593bc77cabd5b8";
    sha256 = "01jqfl5j0vwivdn10ndiaicvp5pwvdg2qy5lrz2jq5r75bihyvsi";
  };

  buildInputs = [ makeWrapper php ];

  buildPhase = let
    quoteMatch = "[\"'\\'" + "']";
    reget = name: "\\$_GET *\\[ *${quoteMatch}${name}${quoteMatch} *\\]";
  in ''
    (
      sed -e 's/pg_connect([^)]*)/pg_connect("dbname=xmppoke")/' \
          -e '/include *(/d' common.php
      sed -e '1{/<?php/d}' -e '/include *(/d' \
          -e '/<div.*navbar/,/^$/d' \
          -e '/>Permalink/d' -e '/>Retest/d' -e '/<button.*Show PEM/d' \
          -e '/<h3>Badge/,/<\/pre>/d' result.php
    ) > getreport.php
    sed -i -e 's/${reget "type"}/$argv[1]/g' \
           -e 's/${reget "domain"}/$argv[2]/g' \
           getreport.php
    php -l getreport.php
  '';

  installPhase = ''
    outfile="$out/libexec/xmppoke-frontend/getreport.php"
    install -vD getreport.php "$outfile"

    mkdir -p "$out/share/xmppoke-frontend"
    cp -rt "$out/share/xmppoke-frontend" js css

    mkdir -p "$out/bin"
    cat > "$out/bin/xmppoke-genreport" <<RUNNER
    #!${stdenv.shell} -e
    if [ -n "\$1" -a -n "\$2" ]; then
      if tmpdir="\$(${coreutils}/bin/mktemp -d)"; then
        trap "rm -rf '\$tmpdir'" EXIT
        "${coreutils}/bin/mkdir" "\$tmpdir/xmppoke"
        "${php}/bin/php" "$outfile" "\$1" "\$2" > "\$tmpdir/xmppoke/\$1.html"
        "${coreutils}/bin/cp" -r --no-preserve=all \
          "$out/share/xmppoke-frontend/js" \
          "$out/share/xmppoke-frontend/css" \
          "\$tmpdir/xmppoke/"
        "${gnutar}/bin/tar" cf "\$1.tar" -C "\$tmpdir" xmppoke
        exit 0
      else
        echo "Unable to create temporary directory." >&2
        exit 1
      fi
    else
      echo "Usage: \$0 TYPE FQDN" >&2
      exit 1
    fi
    RUNNER
    chmod +x "$out/bin/xmppoke-genreport"
  '';
}
