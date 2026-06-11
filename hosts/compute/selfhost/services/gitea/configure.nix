{ config, lib, pkgs, self, ... }:
let
  serviceCfg = config.selfhost.services.gitea;
  oidcCfg = config.selfhost.auth.oidc;

  userAccounts = lib.mapAttrsToList (_: u: {
    inherit (u) username email firstName lastName;
    inherit (u.services.gitea) isAdmin;
    sshKeys = [ ];
  }) (lib.filterAttrs (_: u: u.services.gitea.enable) config.selfhost.users);

  serviceAccounts = lib.mapAttrsToList (name: a: {
    username = name;
    email = "${name}@service.localhost";
    firstName = name;
    lastName = "Service";
    isAdmin = false;
    inherit (a.services.gitea) sshKeys;
  }) (lib.filterAttrs (_: a: a.services.gitea.enable) config.custom.serviceAccounts);

  userListFile = pkgs.writeText "gitea-users.json" (builtins.toJSON (userAccounts ++ serviceAccounts));

  gitea-admin = pkgs.writeShellApplication {
    name = "gitea-admin";
    runtimeInputs = [ config.services.gitea.package ];
    text = ''exec sudo -u ${config.services.gitea.user} -- ${config.services.gitea.package}/bin/gitea -c ${config.services.gitea.stateDir}/custom/conf/app.ini "$@"'';
  };
in
{
  selfhost.runtimeSecrets.gitea-admin-password = {
    regenerateIfMissing = false;
    owner = "gitea";
    restartUnits = [ "gitea-configure.service" ];
  };

  environment.systemPackages = [ gitea-admin ];

  systemd.services.gitea-configure = {
    description = "Gitea setup";
    wantedBy = [ "gitea.service" ];
    after = [ "gitea.service" ];
    requires = [ "gitea.service" ];
    partOf = [ "gitea.service" ];
    restartTriggers = [ ./gitea-configure.nu userListFile ];
    startLimitIntervalSec = 300;
    startLimitBurst = 3;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      TimeoutStartSec = 600;
      Restart = "on-failure";
      RestartSec = 10;
      User = config.services.gitea.user;
      Group = config.services.gitea.group;
      WorkingDirectory = config.services.gitea.stateDir;
      SupplementaryGroups = serviceCfg.oidc.systemd.supplementaryGroups;
    };
    environment = {
      GITEA_URL = serviceCfg.url;
      GITEA_ADMIN_PASSWORD_FILE = config.selfhost.runtimeSecrets.gitea-admin-password.path;
      GITEA_CONFIG = "${config.services.gitea.stateDir}/custom/conf/app.ini";
      OIDC_PROVIDER_NAME = oidcCfg.provider.internalName;
      OIDC_DISPLAY_NAME = oidcCfg.provider.displayName;
      OIDC_DISCOVERY_URL = "${oidcCfg.provider.issuerUrl}/.well-known/openid-configuration";
      OIDC_CLIENT_ID_FILE = serviceCfg.oidc.id.file;
      OIDC_CLIENT_SECRET_FILE = serviceCfg.oidc.secret.file;
      GITEA_USERS_FILE = userListFile;
    };
    path = [ config.services.gitea.package pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "gitea-configure" ./gitea-configure.nu}'';
  };
}
