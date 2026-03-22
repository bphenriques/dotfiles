# Tandoor user provisioning (idempotent, runs on every config change).
#
# Creates local users with password auth and assigns them to the default space
# with the configured permission group. Logic lives in tandoor-provision-users.py.
{ config, pkgs, lib, ... }:
let
  serviceCfg = config.custom.homelab.services.tandoor;
  tandoorCfg = config.services.tandoor-recipes;
  pkg = tandoorCfg.package;

  enabledUsers = lib.filterAttrs (_: u: u.services.tandoor.enable) config.custom.homelab.users;
  localUsers = lib.filterAttrs (_: u: u.services.tandoor.passwordFile != null) enabledUsers;

  usersConfigFile = pkgs.writeText "tandoor-users-config.json" (builtins.toJSON (
    lib.mapAttrsToList (_: u: {
      username = u.username;
      email = u.email;
      group = u.services.tandoor.group;
      passwordCredential = "password-${u.username}";
    }) localUsers
  ));
in
{
  systemd.services.tandoor-recipes-provision-users = lib.mkIf (localUsers != {}) {
    description = "Provision Tandoor Recipes users";
    wantedBy = [ "tandoor-recipes.service" ];
    after = [ "tandoor-recipes.service" "tandoor-recipes-superuser.service" ];
    requires = [ "tandoor-recipes.service" "tandoor-recipes-superuser.service" ];
    partOf = [ "tandoor-recipes.service" ];
    restartTriggers = [ usersConfigFile ./tandoor-provision-users.py ];
    startLimitIntervalSec = 120;
    startLimitBurst = 30;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      Restart = "on-failure";
      RestartSec = "2s";
      User = tandoorCfg.user;
      Group = tandoorCfg.group;
      EnvironmentFile = [ serviceCfg.secrets.templates.env.path ];
      WorkingDirectory = "/var/lib/tandoor-recipes";
      LoadCredential = lib.mapAttrsToList (_: u:
        "password-${u.username}:${u.services.tandoor.passwordFile}"
      ) localUsers;
    };
    environment = {
      DB_ENGINE = "django.db.backends.postgresql";
      POSTGRES_HOST = "/run/postgresql";
      POSTGRES_USER = tandoorCfg.user;
      POSTGRES_DB = tandoorCfg.user;
      PYTHONPATH = "${pkg.python.pkgs.makePythonPath pkg.propagatedBuildInputs}:${pkg}/lib/tandoor-recipes";
      TANDOOR_USERS_CONFIG = usersConfigFile;
    };
    script = ''${lib.getExe pkg} shell < ${./tandoor-provision-users.py}'';
  };
}
