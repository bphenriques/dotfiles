{ config, pkgs, ... }:
let
  serviceCfg = config.selfhost.services.cook-recipes;
  recipesDir = config.custom.paths.media.recipes;
  siteDir = "/var/lib/cook-recipes/site";
in
{
  selfhost.services.cook-recipes = {
    displayName = "Recipes";
    meta.homepage = "https://cooklang.org";
    meta.description = "Recipe Collection";
    meta.category = "productivity";
    subdomain = "recipes";
    port = 9080;
    storage = {
      smb = [ "media" ];
      systemdServices = [ "cook-recipes-build" ];
    };
    extraConfig.landingPage.enable = true;
  };

  users.users.cook-recipes = { isSystemUser = true; group = "cook-recipes"; };
  users.groups.cook-recipes = { };

  systemd.services.cook-recipes-build = {
    description = "Build static Cook recipe site";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      User = "cook-recipes";
      SupplementaryGroups = [ config.selfhost.storage.smb.mounts.media.group ];
      StateDirectory = "cook-recipes";
      ExecStartPre = [
        "${pkgs.coreutils}/bin/test -d ${recipesDir}/library"
        "${pkgs.coreutils}/bin/rm -rf ${siteDir}"
      ];
      ExecStart = "${pkgs.cook-cli}/bin/cook build web --base-path ${recipesDir}/library ${siteDir}";
      Restart = "on-failure";
      RestartSec = "1min";
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

  systemd.services.cook-recipes = {
    description = "Cook recipe static site";
    wantedBy = [ "multi-user.target" ];
    after = [ "cook-recipes-build.service" ];
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
