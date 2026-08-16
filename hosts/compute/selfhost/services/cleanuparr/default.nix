{ config, pkgs, self, ... }:
let
  cfg = config.selfhost;
  serviceCfg = cfg.services.cleanuparr;
  ntfyCfg = cfg.services.ntfy;
  dataDir = "/var/lib/cleanuparr";
  img = pkgs.containerImages.cleanuparr;

  cleanuparrUser = {
    name = "cleanuparr";
    uid = 49031;
    group = "cleanuparr";
    gid = 49031;
  };

  settings = {
    dryRun = false;
    blocklistUrl = "https://cleanuparr.pages.dev/static/blacklist_permissive";
    downloadClient = {
      enabled = true;
      name = "Transmission";
      typeName = "Transmission";
      type = "Torrent";
      host = "http://127.0.0.1:${toString cfg.services.transmission.port}";
      urlBase = "/transmission/";
    };
    sonarr = {
      name = "Sonarr";
      url = cfg.services.sonarr.url;
      apiKeyFile = cfg.apps.sonarr.apiKeyFile;
      version = 4;
    };
    radarr = {
      name = "Radarr";
      url = cfg.services.radarr.url;
      apiKeyFile = cfg.apps.radarr.apiKeyFile;
      version = 6;
    };
    queueCleaner.failedImportMaxStrikes = 5;
    stallRule = {
      name = "Stalled";
      enabled = true;
      maxStrikes = 5;
      privacyType = "Public";
      minCompletionPercentage = 0;
      maxCompletionPercentage = 100;
      resetStrikesOnProgress = true; # a torrent that stalls then resumes
      deletePrivateTorrentsFromClient = false;
      changeCategory = false;
    };
    notification = {
      serverUrl = ntfyCfg.url;
      inherit (serviceCfg.integrations.notify) topic;
      tags = "broom";
    };
  };

  settingsFile = pkgs.writeText "cleanuparr-config.json" (builtins.toJSON settings);
in
{
  selfhost = {
    services.cleanuparr = {
      displayName = "Cleanuparr";
      meta.homepage = "https://github.com/Cleanuparr/Cleanuparr";
      meta.description = "Queue Cleanup";
      meta.category = "media automation";
      port = 11011;
      healthcheck.path = "/health";
      access.allowedGroups = [ cfg.groups.admin ];
      access.model = "forwardAuth";
      integrations.notify.topic = "download";
      integrations.homepage.group = "Admin";
      integrations.homepage.icon = "cleanuparr.png";
      extraConfig.landingPage = { enable = true; listed = false; };
    };

    # Cleanuparr has no API-key auth: the reconcile logs in as a real account, so one has to exist.
    runtimeSecrets.cleanuparr-provisioner-password = {
      restartUnits = [ "cleanuparr-configure.service" ];
    };
  };

  users.groups.${cleanuparrUser.group} = { inherit (cleanuparrUser) gid; };
  users.users.${cleanuparrUser.name} = {
    inherit (cleanuparrUser) uid group;
    isSystemUser = true;
  };

  systemd.tmpfiles.rules = [
    "d ${dataDir} 0750 ${cleanuparrUser.name} ${cleanuparrUser.group} -"
  ];

  virtualisation.oci-containers.containers.cleanuparr = {
    image = "${img.image}:${img.version}";
    autoStart = true;

    environment = {
      PORT = toString serviceCfg.port;
      BIND_ADDRESS = "127.0.0.1";
      BASE_PATH = "";
      TZ = config.time.timeZone;
    };
    volumes = [ "${dataDir}:/config" ];
    user = "${toString cleanuparrUser.uid}:${toString cleanuparrUser.gid}";
    extraOptions = [
      "--network=host" # In order to reach Sonarr, Radarr and Transmission
      "--memory=512m"
    ];
  };

  systemd.services.podman-cleanuparr = {
    after = [ "sonarr.service" "radarr.service" "transmission.service" ];
    wants = [ "sonarr.service" "radarr.service" "transmission.service" ];
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "10s";
      RestartMaxDelaySec = "5min";
      RestartSteps = 5;
    };
  };

  systemd.services.cleanuparr-configure = {
    description = "Cleanuparr setup (download client, *arr instances, queue cleaner, malware blocker)";
    wantedBy = [ "multi-user.target" ];
    after = [ "podman-cleanuparr.service" "sonarr-configure.service" "radarr-configure.service" "ntfy-configure.service" ];
    requires = [ "podman-cleanuparr.service" ];
    wants = [ "sonarr-configure.service" "radarr-configure.service" "ntfy-configure.service" ];
    partOf = [ "podman-cleanuparr.service" ];
    restartTriggers = [ settingsFile ./cleanuparr-configure.nu ];
    startLimitIntervalSec = 300;
    startLimitBurst = 3;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      TimeoutStartSec = 600;
      Restart = "on-failure";
      RestartSec = 10;
    };
    environment = {
      CLEANUPARR_URL = serviceCfg.url;
      CLEANUPARR_CONFIG_FILE = settingsFile;
      CLEANUPARR_CREDENTIALS_FILE = cfg.runtimeSecrets.cleanuparr-provisioner-password.path;
      NTFY_TOKEN_FILE = serviceCfg.integrations.notify.tokenFile;
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "cleanuparr-configure" ./cleanuparr-configure.nu}'';
  };

  selfhost.runtimeSecrets = {
    sonarr-api-key.restartUnits = [ "cleanuparr-configure.service" ];
    radarr-api-key.restartUnits = [ "cleanuparr-configure.service" ];
  };
}
