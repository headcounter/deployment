{ lib, writeText, postfix }:

let
  postSed = writeText "postfix-single-config.sed" ''
    /void.*\<mail_conf_suck/ {
      N
      /{/!b
      :ls
      N
      /\n}$/ {
        s/.*//
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
        bo
      }
      bls
    }
    /\<msg_[a-z0-9_]\+ *(/ {
      /) *;/bi
      :lm
      N
      /) *;/ {
        :i
        /%s\/%s.*\<var_config_dir\>/ {
          s,%s/%s,%s,g
          s/\<var_config_dir\>,[^,)]*/var_config_file/g
        }
        bo
      }
      blm
    }
    :o
    s/concatenate(.*\<var_config_dir\>.*MAIN.*);/mystrdup(var_config_file);/g
    s/\<var_config_dir\>/var_config_file/g
    s/using config files in/using config file/g
  '';

in lib.overrideDerivation postfix (drv: {
  postPatch = (drv.postPatch or "") + ''
    find . -path ./src/postconf -prune -o \
      -type f -exec sed -i -f "${postSed}" {} +

    echo "char *var_config_dir;" >> src/postconf/postconf.h
  '';
})
