# Tandoor Recipes initial setup
#
# LIMITATION: The initial space must be created manually via the web setup wizard.
# There is no stable API or CLI to automate this. After completing the wizard once:
# - OIDC users auto-join space 1 with "user" role via SOCIAL_DEFAULT_ACCESS
# - The superuser can manage additional spaces via the web UI
#
{ config, pkgs, self, ... }:
{
  sops.secrets."tandoor/admin/username" = { };
  sops.secrets."tandoor/admin/password" = { };

  # Create superuser non-interactively using Django's built-in support
  systemd.services.tandoor-recipes-superuser = {
    description = "Create Tandoor Recipes superuser";
    wantedBy = [ "multi-user.target" ];
    after = [ "tandoor-recipes.service" ];
    requires = [ "tandoor-recipes.service" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    environment = {
      DJANGO_SUPERUSER_EMAIL = "admin@tandoor.local";
      TANDOOR_ADMIN_USERNAME_FILE = config.sops.secrets."tandoor/admin/username".path;
      TANDOOR_ADMIN_PASSWORD_FILE = config.sops.secrets."tandoor/admin/password".path;
    };
    path = [ pkgs.nushell pkgs.tandoor-recipes ];
    script = ''nu ${self.lib.builders.writeNushellScript "tandoor-recipes-superuser" ./tandoor-recipes-superuser.nu}'';
  };
}
