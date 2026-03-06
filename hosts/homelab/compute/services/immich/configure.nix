{ config, pkgs, lib, self, ... }:
let
  pathsCfg = config.custom.homelab.paths;
  homelabMounts = config.custom.homelab.cifs.mounts;
  serviceCfg = config.custom.homelab.services.immich;

  enabledUsers = config.custom.homelab.enabledUsers.immich;

  # TODO: admin could be removed if Immich supports OIDC-only admin
  # TODO: Sync the OIDC user files for tighter SSO integration (not implemented).
  immichConfigFile = pkgs.writeText "immich-config.json" (builtins.toJSON {
    admin = {
      email = "admin@immich.local";
      name = "Immich Admin";
      passwordFile = serviceCfg.secrets.files.admin-password.path;
    };
    users = lib.mapAttrsToList (_: u: { inherit (u) email name; }) enabledUsers;

    libraries = lib.concatLists (lib.mapAttrsToList (username: u:
      let userPaths = pathsCfg.${username}.photos;
      in [
        { name = "${username}-library"; ownerEmail = u.email; importPaths = [ userPaths.library ]; exclusionPatterns = [ ]; }
        { name = "${username}-inbox"; ownerEmail = u.email; importPaths = [ userPaths.inbox ]; exclusionPatterns = [ ]; }
      ]
    ) enabledUsers);
  });

  # Bind user photo directories into immich for external library imports
  photoBinds = lib.concatLists (lib.mapAttrsToList (username: _:
    let userPaths = pathsCfg.${username}.photos;
    in [
      "${userPaths.library}:/mnt/immich/${username}"
      "${userPaths.inbox}:/mnt/immich/${username}-inbox"
    ]
  ) enabledUsers);
in
{
  # Ensure immich has access to the mounted directories and is set as dependant
  users.users.immich.extraGroups = lib.mapAttrsToList (username: _: homelabMounts."${username}".group) enabledUsers;
  custom.homelab.cifs.mounts = lib.mapAttrs' (username: _:
    lib.nameValuePair username { systemd.dependentServices = [ "immich-server" ]; }
  ) enabledUsers;

  systemd.services.immich-server.serviceConfig.BindPaths = photoBinds;

  systemd.services.immich-configure = {
    description = "Configure Immich users and external libraries";
    wantedBy = [ "multi-user.target" ];
    after = [ "immich-server.service" ];
    requires = [ "immich-server.service" ];
    partOf = [ "immich-server.service" ];
    restartTriggers = [ immichConfigFile ./immich-configure.nu ];
    startLimitIntervalSec = 300;
    startLimitBurst = 3;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      User = config.services.immich.user;
      Group = config.services.immich.group;
      Restart = "on-failure";
      RestartSec = 10;
    };
    environment = {
      IMMICH_URL = serviceCfg.url;
      IMMICH_CONFIG_FILE = immichConfigFile;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "immich-configure" ./immich-configure.nu}'';
  };

  assertions = [
    {
      assertion = lib.all (username: pathsCfg ? ${username}.photos) (lib.attrNames enabledUsers);
      message = "Each enabled Immich user must have custom.homelab.paths.<username>.photos configured.";
    }
    {
      assertion = lib.all (username: homelabMounts ? ${username}) (lib.attrNames enabledUsers);
      message = "Each enabled Immich user must have a matching homelab mount (custom.homelab.cifs.mounts.<username>).";
    }
  ];
}
