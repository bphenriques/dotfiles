# TODO: Add user extension (custom.homelab._userOptionExtensions) for per-user access
# once Papra exposes an API for organization membership/invites.
{ config, lib, pkgs, self, ... }:
let
  cfg = config.custom.homelab;
  serviceCfg = cfg.services.papra;
  oidcCfg = cfg.oidc;
  homelabMounts = cfg.smb.mounts;
  pathsCfg = cfg.paths;
  dataDir = "/var/lib/papra";
  img = pkgs.containerImages.papra;

  providerId = oidcCfg.provider.internalName;

  papraUser = {
    name = "papra";
    uid = 1221;
    group = "papra";
    gid = 1221;
  };
in
{
  custom.homelab.services.papra = {
    displayName = "Papra";
    metadata.description = "Document Management";
    metadata.version = img.version;
    metadata.homepage = img.homepage;
    metadata.category = "Productivity";
    port = 1221;
    access.allowedGroups = [ cfg.groups.admin ];
    oidc = {
      enable = true;
      callbackURLs = [ "${serviceCfg.publicUrl}/api/auth/oauth2/callback/${providerId}" ];
      systemd.dependentServices = [ "podman-papra" ];
    };
    healthcheck.path = "/";
    healthcheck.probeModule = "http_any";
    integrations.homepage.enable = true;

    secrets = {
      files.auth-secret = { rotatable = true; bytes = 48; };
      templates.env.content = ''
        AUTH_SECRET=${serviceCfg.secrets.placeholder.auth-secret}
        AUTH_PROVIDERS_CUSTOMS=[{"providerId":"${providerId}","providerName":"${oidcCfg.provider.displayName}","type":"oidc","discoveryUrl":"${oidcCfg.provider.issuerUrl}/.well-known/openid-configuration","clientId":"${serviceCfg.oidc.id.placeholder}","clientSecret":"${serviceCfg.oidc.secret.placeholder}","scopes":["openid","profile","email"]}]
      '';
      systemd.dependentServices = [ "podman-papra" ];
    };
  };

  # Allow container bridge to reach Traefik for OIDC discovery/token exchange
  networking.firewall.interfaces.podman0.allowedTCPPorts = [ 443 ];

  custom.homelab.smb.mounts.bphenriques.systemd.dependentServices = [ "podman-papra" ];

  users.groups.${papraUser.group} = { gid = papraUser.gid; };
  users.users.${papraUser.name} = {
    uid = papraUser.uid;
    group = papraUser.group;
    isSystemUser = true;
    extraGroups = [ homelabMounts.bphenriques.group ];
  };

  systemd.tmpfiles.rules = [
    "d ${dataDir}/db 0750 ${papraUser.name} ${papraUser.group} -"
  ];

  systemd.services.podman-papra.serviceConfig = {
    Restart = "on-failure";
    RestartSec = "10s";
    RestartMaxDelaySec = "5min";
    RestartSteps = 5;
  };

  virtualisation.oci-containers.containers.papra = {
    image = "${img.image}:${img.version}-rootless";
    autoStart = true;

    environment = {
      APP_BASE_URL = serviceCfg.publicUrl;
      PORT = toString serviceCfg.port;
      SERVER_HOSTNAME = "0.0.0.0";

      # Storage
      DATABASE_URL = "file:/data/db/db.sqlite";
      DOCUMENT_STORAGE_DRIVER = "filesystem";
      DOCUMENT_STORAGE_FILESYSTEM_ROOT = "/data/documents";
      DOCUMENT_STORAGE_USE_LEGACY_STORAGE_KEY_DEFINITION_SYSTEM = "false";
      DOCUMENT_STORAGE_KEY_PATTERN = "{{document.name}}";

      # Folder ingestion (polling required: inotify does not work on CIFS/SMB)
      INGESTION_FOLDER_IS_ENABLED = "true";
      INGESTION_FOLDER_ROOT_PATH = "/data/ingestion";
      INGESTION_FOLDER_WATCHER_USE_POLLING = "true";
      INGESTION_FOLDER_WATCHER_POLLING_INTERVAL_MS = "21600000"; # 6h
      INGESTION_FOLDER_POST_PROCESSING_STRATEGY = "move";

      # Upload / content extraction
      DOCUMENT_STORAGE_MAX_UPLOAD_SIZE = "1073741824"; # 1 GiB
      DOCUMENTS_CONTENT_EXTRACTION_ENABLED = "true";

      # Documents
      DOCUMENTS_OCR_LANGUAGES = "eng,por";

      # Auth - OIDC via Pocket-ID; first user to log in becomes admin
      AUTH_FIRST_USER_AS_ADMIN = "true";
      AUTH_IS_REGISTRATION_ENABLED = "false";
      AUTH_PROVIDERS_EMAIL_IS_ENABLED = "false";
      AUTH_IS_PASSWORD_RESET_ENABLED = "false";
      AUTH_SHOW_LEGAL_LINKS = "false";
      AUTH_PROVIDERS_GITHUB_IS_ENABLED = "false";
      AUTH_PROVIDERS_GOOGLE_IS_ENABLED = "false";
      AUTH_IP_ADDRESS_HEADERS = "x-forwarded-for";

      # Privacy
      POSTHOG_ENABLED = "false";
    };
    environmentFiles = [ serviceCfg.secrets.templates.env.path ];
    ports = [ "127.0.0.1:${toString serviceCfg.port}:${toString serviceCfg.port}" ];
    volumes = [
      "${dataDir}/db:/data/db"
      "${pathsCfg.users.bphenriques.documents.root}/library:/data/documents"
      "${pathsCfg.users.bphenriques.documents.inbox}:/data/ingestion"
    ];
    user = "${toString papraUser.uid}:${toString papraUser.gid}";
    extraOptions = [
      "--memory=1g"
      "--pids-limit=128"
      "--group-add=${toString homelabMounts.bphenriques.gid}"
      "--add-host=${cfg.services.pocket-id.publicHost}:${self.shared.networks.main.hosts.compute}"
      "--add-host=${serviceCfg.publicHost}:${self.shared.networks.main.hosts.compute}"
    ];
  };
}
