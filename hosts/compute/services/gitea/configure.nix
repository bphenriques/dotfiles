{ config, lib, pkgs, self, ... }:
let
  serviceCfg = config.custom.homelab.services.gitea;
  oidcCfg = config.custom.homelab.oidc;

  enabledUsers = lib.filterAttrs (_: u: u.services.gitea.enable) config.custom.homelab.users;

  # Pass the user list as JSON to the configure script. Password values
  # stay on the filesystem (referenced by path, never inlined). Repo
  # permissions are intentionally NOT provisioned here — the user adds
  # accounts to specific repos via the Gitea UI.
  #
  # Each user's `passwordFile` must be readable by the gitea service
  # user. Set that up where the secret is declared, the same way other
  # services (jellyfin, kavita) do it (e.g. `sops.secrets."gitea/agent/password".owner = "gitea";`).
  userList = lib.mapAttrsToList (_: u: {
    inherit (u) username email firstName lastName;
    inherit (u.services.gitea) isAdmin passwordFile;
  }) enabledUsers;

  userListFile = pkgs.writeText "gitea-users.json" (builtins.toJSON userList);

  # Wrapper that hides the `-c $config_path` and the `sudo -u gitea` plumbing
  # so admin one-offs (e.g. `gitea-admin user generate-access-token ...`)
  # are a single command instead of a flag soup.
  gitea-admin = pkgs.writeShellApplication {
    name = "gitea-admin";
    runtimeInputs = [ config.services.gitea.package ];
    text = ''
      exec sudo -u ${config.services.gitea.user} -- \
        ${config.services.gitea.package}/bin/gitea \
        -c ${config.services.gitea.stateDir}/custom/conf/app.ini "$@"
    '';
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
