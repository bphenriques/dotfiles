{ config, lib, ... }:
let
  cfg = config.custom.homelab;
  serviceCfg = cfg.services.grist;
  tinyauthCfg = cfg.services.tinyauth;
  adminUsers = lib.filterAttrs (_: u: u.isAdmin) cfg.users;
  adminEmail = (lib.head (lib.attrValues adminUsers)).email;
  dataDir = "/var/lib/grist";
  version = "1.7.12";
in
{
  imports = [ ./backup.nix ];

  custom.homelab.services.grist = {
    displayName = "Grist";
    metadata.description = "Spreadsheet Database";
    metadata.version = version;
    metadata.homepage = "https://github.com/gristlabs/grist-core";
    metadata.category = "Productivity";
    port = 8484;
    access.allowedGroups = [ config.custom.homelab.groups.admin ];
    forwardAuth.enable = true;
    healthcheck.path = "/";
    healthcheck.probeModule = "http_any";
    integrations.homepage.enable = true;

    secrets = {
      files.session-secret = { rotatable = true; bytes = 32; };
      templates.env.content = ''
        GRIST_SESSION_SECRET=${serviceCfg.secrets.placeholder.session-secret}
      '';
      systemd.dependentServices = [ "podman-grist" ];
    };
  };

  systemd.tmpfiles.rules = [
    "d ${dataDir} 0750 root root -"
  ];

  virtualisation.oci-containers.containers.grist = {
    image = "gristlabs/grist:${version}";
    autoStart = true;

    environment = {
      APP_HOME_URL = serviceCfg.publicUrl;
      APP_DOC_URL = serviceCfg.publicUrl;
      GRIST_SINGLE_ORG = "home";
      GRIST_ORG_IN_PATH = "false";
      GRIST_SANDBOX_FLAVOR = "gvisor";

      # Security
      ALLOWED_WEBHOOK_DOMAINS = "";
      GRIST_FORWARD_AUTH_HEADER = "Remote-Email";
      GRIST_FORWARD_AUTH_LOGIN_PATH = "/auth/login";
      GRIST_FORWARD_AUTH_LOGOUT_PATH = "${tinyauthCfg.publicUrl}/logout";
      GRIST_IGNORE_SESSION = "true";
      GRIST_FORCE_LOGIN = "true";
      GRIST_DEFAULT_EMAIL = adminEmail;
      GRIST_ALLOWED_HOSTS = serviceCfg.publicHost;

      # Privacy
      GRIST_TELEMETRY_LEVEL = "off";
      GRIST_ALLOW_AUTOMATIC_VERSION_CHECKING = "false";

      # Performance
      GRIST_THROTTLE_CPU = "true";
      GRIST_MAX_UPLOAD_ATTACHMENT_MB = "20";
      GRIST_MAX_UPLOAD_IMPORT_MB = "50";

      # Locale
      GRIST_DEFAULT_LOCALE = cfg.locale.language;
      GRIST_DEFAULT_CURRENCY = cfg.locale.currency;

      # Misc
      GRIST_HIDE_UI_ELEMENTS = "billing,createSite,multiSite,multiAccounts,sendToDrive,helpCenter,tutorials,supportGrist";
    };
    environmentFiles = [ serviceCfg.secrets.templates.env.path ];
    ports = [ "127.0.0.1:${toString serviceCfg.port}:8484" ];
    volumes = [
      "${dataDir}:/persist"
    ];
    user = "0:0";
    extraOptions = [
      "--memory=512m"
      "--security-opt=no-new-privileges=false"
      "--cap-add=CHOWN"
      "--cap-add=DAC_OVERRIDE"
      "--cap-add=FOWNER"
      "--cap-add=SETUID"
      "--cap-add=SETGID"
    ];
  };
}
