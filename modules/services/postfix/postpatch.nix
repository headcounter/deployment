{ lib, writeText, postfix, systemd }:

let
  postSed = writeText "postfix-single-config.sed" ''
    # The mail_conf_suck() function is for reading the Postfix main.cf based on
    # CONF_ENV_DIR. We want it to read a single file instead of assuming that
    # everything will be in a specific directory.
    /void.*\<mail_conf_suck/ {
      N # Eat the next line so we can check whether this is a declaration or the
        # actual function.
      /{/!b # It's a declaration, so branch out so we don't run into...
      :ls # ... this loop.
      N
      /\n}$/ {
        # At this point we have captured the whole function body, which we're
        # going to delete...
        s/.*//
        # ... and add our new version of the function.
        r ${writeText "new-mail-conf-suck.c" ''
          void mail_conf_suck(void) {
            char *config_file;

            if (var_config_file)
              myfree(var_config_file);
            if ((config_file = getenv(CONF_ENV_PATH)) == 0)
              var_config_file = concatenate(DEF_CONFIG_DIR, "/", "main.cf",
                                            (char *) 0);
            else
              var_config_file = mystrdup(config_file);

            if (dict_load_file_xt(CONFIG_DICT, var_config_file) == 0)
              msg_fatal("open %s: %m", var_config_file);
          }
        ''}
        bo # Branch out to misc replacements, because at this point it's pretty
           # clear that we don't have a msg_*() function here.
      }
      bls
    }

    # We need to mangle lines like these:
    #
    # msg_fatal("file %s/%s: parameter %s: unknown user name value: %s",
    #           var_config_dir, MAIN_CONF_FILE,
    #           VAR_DEFAULT_PRIVS, var_default_privs);
    #
    # ... and turn them into something like this:
    #
    # msg_fatal("file %s: parameter %s: unknown user name value: %s",
    #           var_config_file,
    #           VAR_DEFAULT_PRIVS, var_default_privs);
    /\<msg_[a-z0-9_]\+ *(/ {
      /) *;/bi # If the function ends within the same line it starts, branch
               # off to the actual mangling immediately...
      :lm # ... otherwise loop until the end.
      N
      /) *;/ {
        :i # Label for single line functions
        /%s\/%s.*\<var_config_dir\>/ {
          # We now have the full function in the pattern space, so we can do
          # replacements accross multiple lines.
          s,%s/%s,%s,g
          s/\<var_config_dir\>,[^,)]*/var_config_file/g
        }
        bo
      }
      blm
    }

    :o # Miscellaneous replacements that only span single words/lines

    # Lines like this one:
    #
    # path = concatenate(DEF_CONFIG_DIR, "/", "main.cf", (char *) 0);
    #
    # ... need to be turned into this:
    #
    # patch = mystrdup(var_config_file);
    s/concatenate(.*\<var_config_dir\>.*MAIN.*);/mystrdup(var_config_file);/g

    # Avoid reporting a single config file in plural form.
    s/using config files in/using config file/g

    # Every other var_config_dir needs to be replaced as well, including its
    # declaration. This might be too generic but we still have the compiler as
    # a safety net.
    s/\<var_config_dir\>/var_config_file/g
  '';

in lib.overrideDerivation postfix (drv: {
  NIX_CFLAGS_COMPILE = lib.flatten [
    (drv.NIX_CFLAGS_COMPILE or [])
    "-I${systemd.dev or systemd}/include"
  ];

  postPatch = (drv.postPatch or "") + ''
    # Run the sed expression over all files except for postconf.
    #
    # The reason we don't want to replace postconf stuff as well is because it
    # tries to modify not only main.cf but also master.cf.
    #
    # We don't use master.cf and we also don't use postconf (our config files
    # are immutable anyway), so we just make sure that it compiles by adding a
    # declaration for var_config_dir to the postconf header file, because we
    # have replaced var_config_dir with var_config_file in mail_params.h
    # already.
    find . -path ./src/postconf -prune -o \
      -type f -exec sed -i -f "${postSed}" {} +
    echo "char *var_config_dir;" >> src/postconf/postconf.h

    # Prevent services from picking up MASTER_STATUS_FD, because it clashes with
    # systemd-provided FDs.
    sed -i -e '/MASTER_STATUS_FD/d' src/master/*_server.c

    # Use the constant defined by SD_LISTEN_FDS_START as the MASTER_LISTEN_FD.
    # In theory we could just use 3 here, but to make sure this doesn't change
    # in newer systemd versions, let's use the constant from "sd-daemon.h".
    sed -i -e '/^#define.*MASTER_LISTEN_FD/ {
      c #include <systemd/sd-daemon.h> \
        #define MASTER_LISTEN_FD SD_LISTEN_FDS_START
    }' src/master/master_proto.h

    # Don't try to read global config file during postmap.
    sed -i -e '/mail_conf_read/,/mail_dict_init/d' src/postmap/postmap.c
  '';
})
