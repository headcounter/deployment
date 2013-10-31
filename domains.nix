let
  privateKeys = import ./ssl/private.nix;
  publicKeys = import ./ssl/public.nix;

  withSSL = attrs: attrs // {
    ssl.privateKey = builtins.getAttr attrs.fqdn privateKeys;
    ssl.publicKey = builtins.getAttr attrs.fqdn publicKeys;
    ssl.intermediateCert = builtins.readFile ./ssl/intermediate.crt;
  };
in {
  imports = [ ./modules/vhosts.nix ];

  vhosts = {
    headcounter = withSSL {
      fqdn = "headcounter.org";
      ipv4 = "78.47.32.129";
      ipv6 = "2a01:4f8:162:4187::1";
    };

    jabber = withSSL {
      fqdn = "jabber.headcounter.org";
      ipv4 = "78.47.32.130";
      ipv6 = "2a01:4f8:162:4187::2";
    };

    aszlig = withSSL {
      fqdn = "aszlig.net";
      ipv4 = "78.47.32.131";
      ipv6 = "2a01:4f8:162:4187::3";
    };

    noicq = withSSL {
      fqdn = "noicq.org";
      ipv4 = "78.47.32.132";
      ipv6 = "2a01:4f8:162:4187::4";
    };

    no_icq = withSSL {
      fqdn = "noicq.org";
      ipv4 = "78.47.32.133";
      ipv6 = "2a01:4f8:162:4187::5";
    };
  };
}
