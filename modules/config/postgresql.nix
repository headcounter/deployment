{ config, pkgs, lib, ... }:

let
  inherit (lib) mkOption types;

  allPerms = {
    select = {
      sql = "SELECT";
      descriptions = {
        table = "Allows <literal>SELECT</literal> from any column.";
        sequence = ''
          Allows the use of the <function>currval</function> function.
        '';
      };
    };

    insert = {
      sql = "INSERT";
      descriptions = {
        table = "Allows <literal>INSERT</literal> of a new row.";
      };
    };

    update = {
      sql = "UPDATE";
      descriptions = {
        table = "Allows <literal>UPDATE</literal> of any column.";
        sequence = ''
          Allows the use of the <function>nextval</function> function.
        '';
      };
    };

    delete = {
      sql = "DELETE";
      descriptions = {
        table = "Allows <literal>DELETE</literal> of a row.";
      };
    };

    truncate = {
      sql = "TRUNCATE";
      descriptions = {
        table = "Allows <literal>TRUNCATE</literal> of all tables.";
      };
    };

    references = {
      sql = "REFERENCES";
      descriptions = {
        table = "Allows to create a foreign key constraint.";
      };
    };

    trigger = {
      sql = "TRIGGER";
      descriptions = {
        table = "Allows the creation of triggers.";
      };
    };

    create = {
      sql = "CREATE";
      descriptions = {
        database = "Allows new schemas to be created within the database.";
        schema = "Allows new objects to be created within the schema.";
      };
    };

    connect = {
      sql = "CONNECT";
      descriptions = {
        database = "Allows the user to connect to the database.";
      };
    };

    temporary = {
      sql = "TEMPORARY";
      descriptions = {
        database = ''
          Allows temporary tables to be created while using the database.
        '';
      };
    };

    execute = {
      sql = "EXECUTE";
      descriptions = {
        function = "Allows the use of the function.";
      };
    };

    usage = {
      sql = "USAGE";
      descriptions = {
        schema = "Allows access to objects contained in the schema.";
        sequence = ''
          Allows the use of the <function>currval</function> and
          <function>nextval</function> functions.
        '';
      };
    };

    all = {
      sql = "ALL PRIVILEGES";
      descriptions = lib.genAttrs [
        "schema" "sequence" "database" "table" "function"
      ] (lib.const "Grant all of the available privileges at once.");
    };
  };

  getPermsFor = type: let
    isApplicable = lib.const (p: p.descriptions ? ${type});
  in lib.filterAttrs isApplicable allPerms;

  mkIdent = ident: "\"${lib.replaceStrings ["\""] ["\"\""] ident}\"";
  mkStr = str: "'${lib.replaceStrings ["'"] ["''"] str}'";

  mkUserPerms = dbname: username: perms: let
    mkGrant = { type, schema }: on: cfg: let
      isActive = perm: lib.const cfg.${perm};
      allOr = x: if cfg.all then { inherit (allPerms) all; } else x;
      activePerms = allOr (lib.filterAttrs isActive (getPermsFor type));
      perms = lib.mapAttrsToList (lib.const (d: d.sql)) activePerms;
      permsSql = lib.concatStringsSep ", " perms;
      maybeSchema = lib.optionalString (schema != null) "${mkIdent schema}.";
      onType = lib.toUpper type;
      onSingle = "${onType} ${maybeSchema}${mkIdent on}";
      onAll = "ALL ${onType}S IN SCHEMA ${mkIdent schema}";
      onSql = if on == null then onAll else onSingle;
      toSql = "TO ${mkIdent username}";
    in lib.optionalString (perms != []) ''
      GRANT ${permsSql} ON ${onSql} ${toSql};
    '';

    mkGrants = { type, schema, on }: let
      attrs = { inherit type schema; };
    in lib.concatStrings (lib.mapAttrsToList (mkGrant attrs) on);

    mkAllOrSingleGrants = { type, onAll, on }: let
      schemas = lib.attrNames (on // onAll);
      mkForAll = schema: mkGrant {
        inherit type schema;
      } null onAll.${schema};
      mkForSingle = schema: let
        objects = on.${schema};
      in mkGrants {
        inherit type schema;
        on = objects;
      };
      mkDecision = schema:
        if onAll ? ${schema} then mkForAll schema
        else if on ? ${schema} then mkForSingle schema
        else throw "Couldn't find definitions for schema `${schema}'.";
    in lib.concatMapStrings mkDecision schemas;

  in lib.concatStrings [
    (mkGrants {
      type = "schema";
      schema = null;
      on = perms.schema;
    })

    (mkAllOrSingleGrants {
      type = "table";
      onAll = perms.allTables;
      on = perms.table;
    })

    (mkAllOrSingleGrants {
      type = "sequence";
      onAll = perms.allSequences;
      on = perms.sequence;
    })

    (mkAllOrSingleGrants {
      type = "function";
      onAll = perms.allFunctions;
      on = perms.function;
    })

    (mkGrant {
      type = "database";
      schema = null;
    } dbname perms.database)
  ];

  mkUserSql = dbname: uname: cfg: let
    setOwner = "ALTER DATABASE ${mkIdent dbname} OWNER TO ${mkIdent uname};";
    setPerms = mkUserPerms dbname uname cfg.permissions;
  in "${if cfg.isOwner then setOwner else setPerms}\n";

  mkSqlScript = dbname: cfg: let
    userSql = lib.mapAttrsToList (mkUserSql dbname) cfg.users;
    includeSchema = "\\include ${mkStr (toString cfg.schemaFile)}\n";
    customSqlFile = lib.optional (cfg.schemaFile != null) includeSchema;
    extraSql = lib.optional (cfg.extraSql != "") (cfg.extraSql + "\n");
    initialSql = "REVOKE ALL PRIVILEGES ON SCHEMA public FROM PUBLIC;\n";
    content = lib.concatStrings (customSqlFile ++ extraSql ++ userSql);
  in pkgs.writeText "init-${dbname}.sql" (initialSql + content);

  psql = let
    hummingbird =  lib.versionAtLeast config.system.stateVersion "17.09";
    binary = "${config.services.postgresql.package}/bin/psql";
    superuser = if hummingbird then "postgres" else "root";
  in "${binary} -U ${superuser} -b -X -v ON_ERROR_STOP=1";

  mkDbService = name: cfg: {
    name = "postgresql-database-${name}";
    value = {
      description = "Initialize PostgreSQL Database `${name}'";
      wantedBy = [ "multi-user.target" ];
      requires = [ "postgresql-user-roles-init.service" ];
      after = [ "postgresql.service" "postgresql-user-roles-init.service" ];
      serviceConfig.Type = "oneshot";
      serviceConfig.RemainAfterExit = true;
      script = let
        # Use count here, so that we *always* return successfully.
        sql = "SELECT COUNT(*) FROM pg_database WHERE datname=${mkStr name}";
        checkDb = "${psql} -tAq -c ${lib.escapeShellArg sql} postgres";
      in ''
        dropdb() {
          ${psql} -c ${lib.escapeShellArg "DROP DATABASE ${mkIdent name}"} \
            postgres
        }

        if [ "$(${checkDb})" = 0 ]; then
          trap dropdb ERR
          ${psql} -c ${lib.escapeShellArg "CREATE DATABASE ${mkIdent name}"} \
            postgres
          ${psql} -1 -f ${lib.escapeShellArg (mkSqlScript name cfg)} \
            ${lib.escapeShellArg name}
        fi
      '';
    } // lib.optionalAttrs (cfg.neededBy != []) {
      requiredBy = cfg.neededBy;
      before = cfg.neededBy;
    };
  };

  mkPerms = type: types.submodule {
    options = lib.mapAttrs (lib.const (permAttrs: mkOption {
      type = types.bool;
      default = false;
      description = permAttrs.descriptions.${type};
    })) (getPermsFor type);
  };

  permissionOptions.options = {
    schema = mkOption {
      type = types.attrsOf (mkPerms "schema");
      default = {};
      example = {
        myschema.create = true;
        myschema.usage = true;
        public.usage = false;
      };
      description = ''
        The keys of the attribute are the schema names that the permissions
        defined in the values should apply to.
      '';
    };

    table = mkOption {
      type = types.attrsOf (types.attrsOf (mkPerms "table"));
      default = {};
      example = {
        public.users.select = true;
        myschema.passwords.update = true;
      };
      description = ''
        The keys of the top-level attribute set define the schema and the keys
        of the second one define the table names the permissions will apply to.

        So in the example the <literal>users</literal> table within the
        <literal>public</literal> schema will only have the
        <literal>SELECT</literal> permission while the
        <literal>passwords</literal> table within the
        <literal>myschema</literal> schema will have <literal>UPDATE</literal>
        permission.
      '';
    };

    allTables = mkOption {
      type = types.attrsOf (mkPerms "table");
      default = {};
      example = {
        public.select = true;
        myschema.update = true;
      };
      description = ''
        Similar to the <option>table</option> but applies to all tables within
        the specified schema.
      '';
    };

    sequence = mkOption {
      type = types.attrsOf (types.attrsOf (mkPerms "sequence"));
      default = {};
      example = {
        public.mysequence.select = true;
        private.anothersequence.usage = true;
      };
      description = ''
        The keys of the top-level attribute set define the schema and the keys
        of the second one define the sequence names the permissions will apply
        to.

        In the example, we have a sequence <literal>mysequence</literal>
        within the <literal>public</literal> schema which only allows
        <literal>SELECT</literal> (which in turn means that you can only use
        <function>currval</function> to query its value) while the sequence
        <literal>anothersequence</literal> allows <literal>USAGE</literal>
        (which in turn means usage of both <function>currval</function> and
        <function>nextval</function>).
      '';
    };

    allSequences = mkOption {
      type = types.attrsOf (mkPerms "sequence");
      default = {};
      description = ''
        Similar to the <option>sequence</option> but applies to all sequences
        within the specified schema.
      '';
    };

    function = mkOption {
      type = types.attrsOf (types.attrsOf (mkPerms "function"));
      default = {};
      example = {
        public.muchfun.execute = true;
      };
      description = ''
        The keys of the top-level attribute set define the schema and the keys
        of the second one define the function names the permissions will apply
        to.

        In the example we have a function <literal>muchfun</literal> within the
        <literal>public</literal> schema that is allowed to be executed.
      '';
    };

    allFunctions = mkOption {
      type = types.attrsOf (mkPerms "function");
      default = {};
      description = ''
        Similar to the <option>function</option> option but applies to all
        functions within the specified schema.
      '';
    };

    database = lib.mapAttrs (name: permAttrs: mkOption {
      type = types.bool;
      default = false;
      description = permAttrs.descriptions.database;
    }) (getPermsFor "database");
  };

  userOptions.options = {
    password = lib.mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "very-secure";
      description = ''
        The password to set for the user or <literal>null</literal> to not set
        one.

        <note><para>
          <option>login</option> is required to be <literal>true</literal> if
          this option is not <literal>null</literal>.
        </para></note>
      '';
    };

    login = lib.mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to allow the user to be able to log in.
      '';
    };

    isOwner = lib.mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to give the user all privileges to the database.
        If this is set to <literal>true</literal>, the definitions for
        <option>permissions</option> are ignored.
      '';
    };

    permissions = lib.mkOption {
      type = types.submodule permissionOptions;
      default = {
        allFunctions.public.execute = true;
        allSequences.public.usage = true;
        allTables.public.delete = true;
        allTables.public.insert = true;
        allTables.public.select = true;
        allTables.public.truncate = true;
        allTables.public.update = true;
        database.connect = true;
        database.temporary = true;
        schema.public.usage = true;
      };
      description = ''
        The permissions to grant for this user.

        Defaults are set so that the user can only read and write data but is
        not allowed to change the schema.
      '';
    };
  };

  dbOptions.options = {
    users = lib.mkOption {
      type = types.attrsOf (types.submodule userOptions);
      default = {};
      description = ''
        A set of user options that have privileges on this database.
      '';
    };

    neededBy = lib.mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        A list of systemd units that need this database.
      '';
    };

    schemaFile = lib.mkOption {
      type = types.nullOr types.path;
      default = null;
      example = "/some/important/schema.sql";
      description = ''
        An SQL file to apply directly after creating the database.
      '';
    };

    extraSql = lib.mkOption {
      type = types.lines;
      default = "";
      description = ''
        Extra SQL statements to execute after creating the database.
      '';
    };
  };

  opt = "headcounter.postgresql.databases";

  getUniqueUsers = databases: let
    users = lib.mapAttrsToList (lib.const (db: db.users)) databases;
    zipper = name: cfgs: {
      login = lib.all (c: c.login) cfgs;
      password = let
        maybe = lib.filter (c: c.password != null) cfgs;
      in if lib.length maybe > 0 then (lib.head maybe).password else null;
    };
  in lib.zipAttrsWith zipper users;

in {
  options.headcounter.postgresql.databases = lib.mkOption {
    type = types.attrsOf (types.submodule dbOptions);
    default = {};
    description = ''
      An attribute set of databases to create along with user privileges and
      initial schemas.
    '';
  };

  config = lib.mkIf (config.headcounter.postgresql.databases != {}) {
    assertions = let
      mkUserAsserts = dbname: username: cfg: lib.singleton {
        assertion = cfg.password != null -> cfg.login;
        message = "A password is set for `${opt}.${dbname}.${username}' but "
                + "the `login' option is disabled.";
      };

      mkLoginAsserts = user: defs: let
        logins = lib.unique (map (d: d.login) defs);
        filterNulls = lib.filter (v: v != null);
        passwds = lib.unique (filterNulls (map (d: d.password) defs));
        conflicts = lib.concatMapStringsSep ", " (p: "`${p}'") passwds;
      in [
        { assertion = lib.length logins <= 1;
          message = "User `${user}' has conflicting definitions for `login' "
                  + "- they either must be all `false' or all `true'.";
        }
        { assertion = lib.length passwds <= 1;
          message = "User `${user}' has conflicting definitions for "
                  + "`password': ${conflicts}";
        }
      ];

      inherit (config.headcounter.postgresql) databases;

      loginAsserts = let
        users = lib.mapAttrsToList (lib.const (db: db.users)) databases;
        asserts = lib.zipAttrsWith mkLoginAsserts users;
      in lib.concatLists (lib.attrValues asserts);

      mkDbAsserts = dbname: cfg: let
        userAsserts = lib.mapAttrsToList (mkUserAsserts dbname) cfg.users;
        owners = lib.filterAttrs (name: ucfg: ucfg.isOwner) cfg.users;
        ownerNames = lib.attrNames owners;
        mkOwner = owner: "`${owner}'";
        ownersDesc = lib.concatMapStringsSep ", " mkOwner ownerNames;
        dbNameOpt = "`${opt}.${dbname}'";
      in lib.concatLists userAsserts ++ lib.singleton {
        assertion = lib.length (lib.attrNames owners) <= 1;
        message = "The database defined in ${dbNameOpt} has more than one "
                + "owner: ${ownersDesc}";
      };

      topLevel = lib.singleton {
        assertion = config.services.postgresql.enable;
        message = "You have defined PostgreSQL databases in `${opt}' but "
                + "`services.postgresql.enable' is set to `false'.";
      };

      dbAsserts = lib.concatLists (lib.mapAttrsToList mkDbAsserts databases);

    in topLevel ++ dbAsserts ++ loginAsserts;

    systemd.services = {
      postgresql-user-roles-init = {
        description = "Initialize PostgreSQL Users";
        wantedBy = [ "multi-user.target" ];
        after = [ "postgresql.service" ];
        serviceConfig.Type = "oneshot";
        serviceConfig.RemainAfterExit = true;
        script = lib.concatStrings (lib.mapAttrsToList (uname: cfg: let
          # Use count here, so that we *always* return successfully.
          sql = "SELECT COUNT(*) FROM pg_roles WHERE rolname=${mkStr uname}";
          checkUser = "${psql} -tAq -c ${lib.escapeShellArg sql} postgres";

          maybeLogin = lib.optionalString cfg.login " WITH LOGIN";
          passSql = " PASSWORD ${mkStr cfg.password}";
          maybePass = lib.optionalString (cfg.password != null) passSql;
          createSql = "CREATE ROLE ${mkIdent uname}${maybeLogin}${maybePass}";
        in ''
          if [ "$(${checkUser})" = 0 ]; then
            ${psql} -c ${lib.escapeShellArg createSql} postgres
          fi
        '') (getUniqueUsers config.headcounter.postgresql.databases));
      };
    } // lib.mapAttrs' mkDbService config.headcounter.postgresql.databases;
  };
}
