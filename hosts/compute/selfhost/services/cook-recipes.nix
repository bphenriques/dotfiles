{ config, pkgs, ... }:
let
  serviceCfg = config.selfhost.services.cook-recipes;
  recipesDir = config.custom.paths.media.recipes;
  siteDir = "/var/lib/cook-recipes/site";
in
{
  selfhost.services.cook-recipes = {
    displayName = "Recipes";
    description = "Recipe Collection";
    subdomain = "recipes";
    port = 9080;
    access.allowedGroups = [ config.selfhost.groups.users ];
    forwardAuth.enable = true;
    storage.smb = [ "media" ];
  };

  users.users.cook-recipes = { isSystemUser = true; group = "cook-recipes"; };
  users.groups.cook-recipes = { };

  # `cook build web` renders the library to a self-contained static site, excluding the
  # dynamic shopping-list/pantry/editing features. Rebuilt from the SMB library rather than
  # built at eval time (recipes are runtime data, not in the store); the timer below refreshes
  # it since inotify does not work on SMB mounts.
  systemd.services.cook-recipes-build = {
    description = "Build static Cook recipe site";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" "remote-fs.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      User = "cook-recipes";
      SupplementaryGroups = [ config.selfhost.storage.smb.mounts.media.group ];
      StateDirectory = "cook-recipes";
      # Rebuild from scratch so recipes deleted upstream stop being served (cook build web
      # overwrites but does not prune).
      ExecStartPre = "${pkgs.coreutils}/bin/rm -rf ${siteDir}";
      ExecStart = "${pkgs.cook-cli}/bin/cook build web --base-path ${recipesDir}/library ${siteDir}";
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      NoNewPrivileges = true;
      ProtectKernelTunables = true;
      ProtectControlGroups = true;
      RestrictSUIDSGID = true;
      RestrictAddressFamilies = [ "AF_UNIX" ];
    };
  };

  # Same lightweight static-file pattern as bentopdf; darkhttpd serves files fresh from disk,
  # so a rebuild is picked up without restarting it.
  systemd.services.cook-recipes = {
    description = "Cook recipe static site";
    wantedBy = [ "multi-user.target" ];
    after = [ "cook-recipes-build.service" ];
    requires = [ "cook-recipes-build.service" ];
    serviceConfig = {
      ExecStart = "${pkgs.darkhttpd}/bin/darkhttpd ${siteDir} --addr 127.0.0.1 --port ${toString serviceCfg.port} --no-listing --no-server-id";
      User = "cook-recipes";
      StateDirectory = "cook-recipes";
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
    };
  };

  # Refresh the site weekly to pick up recipe changes (inotify does not work on SMB mounts).
  systemd.timers.cook-recipes-build = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "weekly";
      RandomizedDelaySec = "1h";
      Persistent = true;
    };
  };
}
