{ config, pkgs, ... }:
let
  serviceCfg = config.selfhost.services.cook-recipes;
  recipesDir = "${config.fleet.shares.media.root}/recipes";
  stateDir = "/var/lib/cook-recipes";
  siteDir = "${stateDir}/site";   # a symlink to the current build

  build = pkgs.writeShellApplication {
    name = "cook-recipes-build";
    runtimeInputs = [ pkgs.cook-cli pkgs.coreutils pkgs.findutils ];
    text = builtins.readFile ./cook-recipes-build.sh;
  };
in
{
  selfhost.services.cook-recipes = {
    displayName = "Recipes";
    meta.homepage = "https://cooklang.org";
    meta.description = "Recipe Collection";
    meta.category = "productivity";
    subdomain = "recipes";
    access.model = "open"; # a generated static site with no login
    port = 9080;
    systemdServices = [ "cook-recipes-build" ]; # the generator, not a long-running service
    storage.mounts = [ "media" ];
    storage.users = [ "cook-recipes" ];
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
      StateDirectory = "cook-recipes";
      ExecStartPre = "${pkgs.coreutils}/bin/test -d ${recipesDir}/library";
      ExecStart = "${pkgs.lib.getExe build} ${recipesDir}/library ${stateDir}";
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

  # Alfred's plans and the Cookidoo sync write through this host (it serves the VM its share), so their changes raise
  # inotify events here. Edits made straight on the NAS raise none, hence the daily run as well.
  systemd.paths.cook-recipes-build = {
    wantedBy = [ "paths.target" ];
    pathConfig.PathChanged = [ "${recipesDir}/library" "${recipesDir}/library/plans" ];
  };
  systemd.timers.cook-recipes-build = {
    wantedBy = [ "timers.target" ];
    timerConfig = { OnCalendar = "daily"; Persistent = true; };
  };
}
