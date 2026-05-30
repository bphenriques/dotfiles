{ config, lib, pkgs, self, ... }:
let
  serviceCfg = config.custom.homelab.services.gitea;
  oidcCfg = config.custom.homelab.oidc;

  # Pass identities (human users + service accounts) as JSON to the configure
  # script. Auth is OIDC for humans, SSH key/PAT for service accounts — no
  # passwords flow through this script. Repo permissions are intentionally
  # NOT provisioned here; granted by hand in the gitea UI.
  enabledUsers = lib.filterAttrs (_: u: u.services.gitea.enable) config.custom.homelab.users;
  enabledServiceAccounts = lib.filterAttrs (_: a: a.services.gitea.enable) config.custom.homelab.serviceAccounts;

  humanRecords = lib.mapAttrsToList (_: u: {
    inherit (u) username email firstName lastName;
    inherit (u.services.gitea) isAdmin;
    sshKeys = [ ];
  }) enabledUsers;

  # Service accounts get derived placeholder identity fields — gitea's user
  # schema requires email/firstName/lastName, but nothing in the homelab
  # reads them for these accounts.
  serviceRecords = lib.mapAttrsToList (name: a: {
    username = name;
    email = "${name}@service.localhost";
    firstName = name;
    lastName = "Service";
    isAdmin = false;
    inherit (a.services.gitea) sshKeys;
  }) enabledServiceAccounts;

  userList = humanRecords ++ serviceRecords;

  userListFile = pkgs.writeText "gitea-users.json" (builtins.toJSON userList);

  gitea-admin = pkgs.writeShellApplication {
    name = "gitea-admin";
    runtimeInputs = [ config.services.gitea.package ];
    text = ''exec sudo -u ${config.services.gitea.user} -- ${config.services.gitea.package}/bin/gitea -c ${config.services.gitea.stateDir}/custom/conf/app.ini "$@"'';
  };
in
{
  custom.homelab.services.gitea.secrets = {
    files.admin-password = { rotatable = false; };
    systemd.dependentServices = [ "gitea" "gitea-configure" ];
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
      GITEA_ADMIN_PASSWORD_FILE = serviceCfg.secrets.files.admin-password.path;
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
