{
  imports = [ ./modules/vhosts.nix ];

  vhosts = {
    headcounter = {
      fqdn = "headcounter.org";
      ipv4 = "78.47.32.128";
      ipv6 = "2a01:4f8:162:4187::1";
    };

    jabber = {
      fqdn = "jabber.headcounter.org";
      ipv4 = "78.47.32.129";
      ipv6 = "2a01:4f8:162:4187::2";
    };

    aszlig = {
      fqdn = "aszlig.net";
      ipv4 = "78.47.32.130";
      ipv6 = "2a01:4f8:162:4187::3";
    };

    noicq = {
      fqdn = "noicq.org";
      ipv4 = "78.47.32.131";
      ipv6 = "2a01:4f8:162:4187::4";
    };

    no_icq = {
      fqdn = "noicq.org";
      ipv4 = "78.47.32.132";
      ipv6 = "2a01:4f8:162:4187::5";
    };
  };
}
