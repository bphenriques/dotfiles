# Tandoor superuser provisioning (first boot only).
# NOTE: DB env vars duplicated from NixOS tandoor-recipes module (createsuperuser needs full Django env).
{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.tandoor;
  tandoorCfg = config.services.tandoor-recipes;
  pkg = tandoorCfg.package;
in
{
  custom.homelab.services.tandoor.secrets.files.admin-password = { rotatable = false; };

  systemd.services.tandoor-recipes-superuser = {
    description = "Create Tandoor Recipes superuser";
    wantedBy = [ "tandoor-recipes.service" ];
    after = [ "tandoor-recipes.service" ];
    requires = [ "tandoor-recipes.service" ];
    partOf = [ "tandoor-recipes.service" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      User = tandoorCfg.user;
      Group = tandoorCfg.group;
      EnvironmentFile = [ serviceCfg.secrets.templates.env.path ];
      WorkingDirectory = "/var/lib/tandoor-recipes";
      ConditionPathExists = "!/var/lib/tandoor-recipes/.superuser-created";
      LoadCredential = [ "admin-password:${serviceCfg.secrets.files.admin-password.path}" ];
    };
    environment = {
      DJANGO_SUPERUSER_USERNAME = "admin";
      DJANGO_SUPERUSER_EMAIL = "admin@tandoor.local";
      DB_ENGINE = "django.db.backends.postgresql";
      POSTGRES_HOST = "/run/postgresql";
      POSTGRES_USER = tandoorCfg.user;
      POSTGRES_DB = tandoorCfg.user;
      PYTHONPATH = "${pkg.python.pkgs.makePythonPath pkg.propagatedBuildInputs}:${pkg}/lib/tandoor-recipes";
    };
    script = ''
      set -euo pipefail
      export DJANGO_SUPERUSER_PASSWORD="$(tr -d '\n' < "$CREDENTIALS_DIRECTORY/admin-password")"

      for i in $(seq 1 30); do
        echo "Creating superuser (attempt $i)..."
        output="$(${pkg}/bin/tandoor-recipes createsuperuser --noinput 2>&1)" && break
        if echo "$output" | grep -q "That username is already taken"; then
          echo "Superuser already exists, skipping creation."
          break
        fi
        sleep 2
      done

      touch /var/lib/tandoor-recipes/.superuser-created
    '';
  };
}
