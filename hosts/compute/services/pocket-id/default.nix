{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.home-server.services.pocket-id;
  encryptionKeyFile = "/var/lib/pocket-id/encryption.key";
  apiKeyFile = config.sops.templates."pocket-id-api-key".path;
  smtpPasswordFile = config.sops.templates."pocket-id-smtp-password".path;

  configFile = pkgs.writeText "pocket-id-config.json" (builtins.toJSON { inherit users groups; });

  # Filter users with pocket-id enabled and format for init script
  enabledUsers = lib.filterAttrs (_: u: u.services.pocket-id.enable) config.custom.home-server.users;
  users = lib.mapAttrsToList (_: u: {
    inherit (u) username email;
    firstName = u.name;
    lastName = "";
    groups = u.services.pocket-id.groups;
    isAdmin = builtins.elem "admins" u.services.pocket-id.groups;
  }) enabledUsers;

  # Collect all unique groups from users
  allGroups = lib.unique (lib.concatLists (lib.mapAttrsToList (_: u: u.services.pocket-id.groups) enabledUsers));
  groups = map (name: { inherit name; }) allGroups;
in
{
  custom.home-server.services.pocket-id.port = 8082;

  services.pocket-id = {
    enable = true;
    settings = {
      APP_URL = serviceCfg.publicUrl;
      PORT = toString serviceCfg.port;
      HOST = "127.0.0.1";
      TRUST_PROXY = true;
      ANALYTICS_DISABLED = true;
      ENCRYPTION_KEY_FILE = encryptionKeyFile;
      ACCENT_COLOR = "default";
      STATIC_API_KEY_FILE = apiKeyFile;

      # SMTP configuration
      SMTP_HOST = self.settings.smtp.host;
      SMTP_PORT = toString self.settings.smtp.port;
      SMTP_FROM = self.settings.smtp.from;
      SMTP_USER = self.settings.smtp.user;
      SMTP_TLS = self.settings.smtp.tls;
      SMTP_PASSWORD_FILE = smtpPasswordFile;

      # Enable admin-triggered invite emails
      EMAIL_ONE_TIME_ACCESS_AS_ADMIN_ENABLED = "true";
    };
  };

  sops = {
    secrets.pocket_id_api_key = { };
    secrets.pocket_id_smtp_password = { };
    templates."pocket-id-api-key" = {
      owner = config.services.pocket-id.user;
      content = config.sops.placeholder.pocket_id_api_key;
    };
    templates."pocket-id-smtp-password" = {
      owner = config.services.pocket-id.user;
      content = config.sops.placeholder.pocket_id_smtp_password;
    };
  };

  systemd.services.pocket-id = {
    preStart = ''
      if [ ! -f "${encryptionKeyFile}" ]; then
        mkdir -p "$(dirname "${encryptionKeyFile}")"
        ${lib.getExe pkgs.openssl} rand -base64 32 > "${encryptionKeyFile}"
        chown ${config.services.pocket-id.user}:${config.services.pocket-id.group} "${encryptionKeyFile}"
      fi
    '';
  };

  # Initialize users and groups after Pocket ID starts
  systemd.services.pocket-id-init = {
    description = "Initialize Pocket ID users and groups";
    wantedBy = [ "multi-user.target" ];
    after = [ "pocket-id.service" ];
    requires = [ "pocket-id.service" ];
    partOf = [ "pocket-id.service" ];
    serviceConfig = {
      Type = "oneshot";
      User = config.services.pocket-id.user;
      Group = config.services.pocket-id.group;
      Restart = "on-failure";
      RestartSec = 10;
      StartLimitIntervalSec = 300;
      StartLimitBurst = 3;
    };
    environment = {
      POCKET_ID_URL = serviceCfg.internalUrl;
      POCKET_ID_API_KEY_FILE = apiKeyFile;
      POCKET_ID_CONFIG_FILE = configFile;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${./pocket-id-init.nu}'';
  };
  # CLI wrappers for admin scripts (run as root)
  environment.systemPackages = let
    commonEnv = ''
      export POCKET_ID_URL="${serviceCfg.internalUrl}"
      export POCKET_ID_API_KEY_FILE="${apiKeyFile}"
      export POCKET_ID_CONFIG_FILE="${configFile}"
    '';
  in [
    (pkgs.writeShellScriptBin "pocket-id-invite" ''
      ${commonEnv}
      exec ${lib.getExe pkgs.nushell} ${./pocket-id-invite.nu} "$@"
    '')
    (pkgs.writeShellScriptBin "pocket-id-drift" ''
      ${commonEnv}
      exec ${lib.getExe pkgs.nushell} ${./pocket-id-drift.nu}
    '')
  ];
}