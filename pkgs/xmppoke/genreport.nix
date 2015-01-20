{ stdenv, coreutils, gnutar, fetchhg, makeWrapper, php }:

stdenv.mkDerivation {
  name = "xmppoke-genreport";

  src = fetchhg {
    url = "https://bitbucket.org/xnyhps/xmppoke-frontend";
    rev = "25adccac40f5909c4791f774ffb90081303308be";
    sha256 = "0q5fj63y16jlkrymsk7lsaf413fgc2w3k3y116ly9c5x064ycs9y";
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
          -e '/>Permalink/d' -e '/>Retest/d' \
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
