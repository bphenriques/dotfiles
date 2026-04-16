{ config, pkgs, lib, self, ... }:
let
  pathsCfg = config.custom.homelab.paths;
  homelabMounts = config.custom.homelab.smb.mounts;
  serviceCfg = config.custom.homelab.services.immich;

  enabledUsers = lib.filterAttrs (_: u: u.services.immich.enable) config.custom.homelab.users;
  usersWithMounts = lib.filterAttrs (username: _: homelabMounts ? ${username}) enabledUsers;

  # Admin user for API bootstrapping; OIDC users get admin via isAdmin flag
  immichConfigFile = pkgs.writeText "immich-config.json" (builtins.toJSON {
    admin = {
      email = "admin@immich.local";
      name = "Immich Admin";
      passwordFile = serviceCfg.secrets.files.admin-password.path;
    };
    users = lib.mapAttrsToList (_: u: { inherit (u) email name isAdmin; }) enabledUsers;

    libraries = lib.concatLists (lib.mapAttrsToList (username: u:
      let userPaths = pathsCfg.users.${username}.photos;
      in [
        { name = "${username}-library"; ownerEmail = u.email; importPaths = [ userPaths.library ]; exclusionPatterns = [ ]; }
        { name = "${username}-inbox"; ownerEmail = u.email; importPaths = [ userPaths.inbox ]; exclusionPatterns = [ ]; }
      ]
    ) usersWithMounts);
  });

  # Photo directories immich needs access to for external library imports
  photoPaths = lib.concatLists (lib.mapAttrsToList (username: _:
    let userPaths = pathsCfg.users.${username}.photos;
    in [ userPaths.library userPaths.inbox ]
  ) usersWithMounts);
in
{
  # Ensure immich has access to the mounted directories and is set as dependant
  users.users.immich.extraGroups = lib.mapAttrsToList (username: _: homelabMounts.${username}.group) usersWithMounts;
  custom.homelab.smb.mounts = lib.mapAttrs' (username: _:
    lib.nameValuePair username { systemd.dependentServices = [ "immich-server" ]; }
  ) enabledUsers;

  systemd.services.immich-server.serviceConfig.ReadWritePaths = photoPaths;

  systemd.services.immich-configure = {
    description = "Immich setup";
    wantedBy = [ "immich-server.service" ];
    after = [ "immich-server.service" ];
    requires = [ "immich-server.service" ];
    partOf = [ "immich-server.service" ];
    restartTriggers = [ immichConfigFile ./immich-configure.nu ];
    startLimitIntervalSec = 300;
    startLimitBurst = 3;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      TimeoutStartSec = 600;
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

  assertions = let
    missingMounts = lib.filter (u: !(homelabMounts ? ${u})) (lib.attrNames enabledUsers);
  in [{
    assertion = missingMounts == [];
    message = "Immich-enabled users missing SMB mounts: ${toString missingMounts}";
  }];
}
