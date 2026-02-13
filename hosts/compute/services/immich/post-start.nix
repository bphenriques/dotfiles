# 5. OIDC USERS SYNC: Could optionally use oidcCfg.credentials.usersFile to import OIDC subject
#    IDs into Immich user metadata for tighter SSO integration (not implemented).
#
{ config, pkgs, lib, self, ... }:
let
  pathsCfg = config.custom.paths;
  homelabMounts = config.custom.fileSystems.homelab.mounts;
  serviceCfg = config.custom.home-server.services.immich;
  oidcClient = config.custom.home-server.oidc.clients.immich;
  oidcCfg = config.custom.home-server.oidc;

  enabledUsers = config.custom.home-server.enabledUsers.immich;

  credentialsDir = "/var/lib/immich/credentials";

  immichConfigJson = builtins.toJSON {
    admin = {
      email = "admin@immich.local";
      name = "Immich Admin";
      passwordFile = "${credentialsDir}/admin-password";
    };
    users = lib.mapAttrsToList (_: u: { inherit (u) email name; }) enabledUsers;

    libraries = lib.concatLists (lib.mapAttrsToList (username: u:
      let userPaths = pathsCfg.${username}.photos;
      in [
        { name = "${username}-library"; ownerEmail = u.email; importPaths = [ userPaths.library ]; exclusionPatterns = [ ]; }
        { name = "${username}-inbox"; ownerEmail = u.email; importPaths = [ userPaths.inbox ]; exclusionPatterns = [ ]; }
      ]
    ) enabledUsers);
  };

  nasUnits = lib.unique (lib.mapAttrsToList (username: _: homelabMounts.${username}.automountUnit) enabledUsers);

  photoBinds = lib.concatLists (lib.mapAttrsToList (username: _:
    let userPaths = pathsCfg.${username}.photos;
    in [
      "${userPaths.library}:/mnt/media/${username}"
      "${userPaths.inbox}:/mnt/media/${username}-inbox"
    ]
  ) enabledUsers);
in
{
  # Ensure immich has access to the mounted directories.
  users.users.immich.extraGroups = lib.mapAttrsToList (username: _: homelabMounts."${username}".group) enabledUsers;
  systemd.tmpfiles.rules = [
    "d ${credentialsDir} 0700 ${config.services.immich.user} ${config.services.immich.group} -"
  ];

  systemd.services.immich-server = {
    requires = nasUnits;
    after = nasUnits ++ [ oidcCfg.systemd.provisionedTarget ];
    wants = [ oidcCfg.systemd.provisionedTarget ];
    serviceConfig = {
      SupplementaryGroups = [ oidcClient.group ];
      BindPaths = photoBinds;
    };
  };

  systemd.services.immich-configure = {
    description = "Configure Immich users and external libraries";
    wantedBy = [ "multi-user.target" ];
    after = [ "immich-server.service" ];
    requires = [ "immich-server.service" ];
    partOf = [ "immich-server.service" ];
    restartTriggers = [ immichConfigJson ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      User = config.services.immich.user;
      Group = config.services.immich.group;
      Restart = "on-failure";
      RestartSec = 10;
      StartLimitBurst = 3;
    };
    environment = {
      IMMICH_URL = serviceCfg.internalUrl;
      IMMICH_CONFIG_FILE = pkgs.writeText "immich-config.json" immichConfigJson;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "immich-configure" ./immich-configure.nu}'';
  };

  assertions = [
    {
      assertion = lib.all (username: pathsCfg ? ${username} && pathsCfg.${username} ? photos) (lib.attrNames enabledUsers);
      message = "Each enabled Immich user must have custom.paths.<username>.photos configured.";
    }
    {
      assertion = lib.all (username: homelabMounts ? ${username}) (lib.attrNames enabledUsers);
      message = "Each enabled Immich user must have a matching homelab mount (custom.fileSystems.homelab.mounts.<username>).";
    }
  ];
}
