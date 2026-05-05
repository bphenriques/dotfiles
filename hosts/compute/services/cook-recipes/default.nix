{ config, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.cook-recipes;
  recipesDir = config.custom.homelab.paths.media.recipes;
in
{
  custom.homelab.services.cook-recipes = {
    displayName = "Recipes";
    metadata.description = "Recipe Collection";
    metadata.version = pkgs.cook-cli.version;
    metadata.homepage = pkgs.cook-cli.meta.homepage;
    metadata.category = "Home";
    subdomain = "recipes";
    port = 9080;
    access.allowedGroups = [ config.custom.homelab.groups.users ];
    forwardAuth.enable = true;
    integrations.homepage.enable = true;
    storage.smb = [ "media" ];
  };

  systemd.services.cook-recipes = {
    description = "Cook recipe server";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.cook-cli}/bin/cook server --port ${toString serviceCfg.port} ${recipesDir}/library";
      DynamicUser = true;
      SupplementaryGroups = [ config.custom.homelab.smb.mounts.media.group ];
      Restart = "on-failure";
      RestartSec = "10s";
      RestartMaxDelaySec = "5min";
      RestartSteps = 5;
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      NoNewPrivileges = true;
      ProtectKernelTunables = true;
      ProtectControlGroups = true;
      RestrictSUIDSGID = true;
      RestrictAddressFamilies = [ "AF_INET" "AF_INET6" ];
      ReadWritePaths = [ "${recipesDir}/library" ];
    };
  };

  # Periodic restart to pick up recipe changes (inotify does not work on SMB mounts)
  systemd.timers.cook-recipes-restart = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "weekly";
      RandomizedDelaySec = "1h";
      Persistent = true;
    };
  };

  systemd.services.cook-recipes-restart = {
    description = "Restart Cook recipe server to pick up changes";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.systemd}/bin/systemctl restart cook-recipes.service";
    };
  };
}
