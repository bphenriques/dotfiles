{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.immich;
  oidcCfg = config.custom.homelab.oidc;
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.immich = {
    displayName = "Immich";
    metadata.description = "Photo & Video Gallery";
    metadata.version = config.services.immich.package.version;
    metadata.homepage = config.services.immich.package.meta.homepage;
    metadata.category = "Media";
    port = 2283;
    subdomain = "photos";
    secrets = {
      files.admin-password = { rotatable = false; };
      systemd.dependentServices = [ "immich-configure" ];
    };

    access.allowedGroups = with config.custom.homelab.groups; [ admin ];
    oidc = {
      enable = true;
      callbackURLs = [
        "${serviceCfg.publicUrl}/auth/login"
        "${serviceCfg.publicUrl}/user-settings"
        "app.immich:///oauth-callback"
      ];
      systemd.dependentServices = [ "immich-server" ];
    };

    healthcheck.path = "/api/server/ping";
    integrations.homepage.enable = true;
    resourceControl = {
      slice = "throttled";
      systemdServices = [ "immich-server" "immich-machine-learning" ];
    };
  };

  services.immich = {
    enable = true;
    inherit (serviceCfg) host;
    inherit (serviceCfg) port;
    mediaLocation = "/var/lib/immich";

    settings = {
      server.externalDomain = serviceCfg.publicUrl;
      newVersionCheck.enabled = false;

      oauth = {
        enabled = true;
        inherit (oidcCfg.provider) issuerUrl;
        clientId._secret = serviceCfg.oidc.id.file;
        clientSecret._secret = serviceCfg.oidc.secret.file;
        scope = "openid email profile";
        signingAlgorithm = "RS256";
        buttonText = "Login with ${oidcCfg.provider.displayName}";
        autoRegister = true;
        autoLaunch = false;
      };
      passwordLogin.enabled = true; # TODO: review whether this is still needed after OIDC is fully rolled out
      library.watch.enabled = true;

      ffmpeg = {
        accel = "disabled";      # CPU-only; iGPU reserved for Jellyfin and I can't throttle GPU. Both will lead to thermal issues in such as small device.
        accelDecode = false;
        acceptedVideoCodecs = [ "h264" "hevc" ];
        threads = 2;
      };

      storageTemplate = {
        enabled = true;
        hashVerificationEnabled = true;
        template = "{{y}}/{{y}}-{{MM}}-{{dd}}/{{filename}}";
      };

      # Reduce job concurrency: ML for memory pressure, thumbnails for CPU/thermal
      job = {
        videoConversion.concurrency = 1;
        thumbnailGeneration.concurrency = 1;
        faceDetection.concurrency = 1;
        smartSearch.concurrency = 1;
      };

      # Spread nightly work
      library.scan.cronExpression = "0 3 * * *";
      nightlyTasks.startTime = "02:00";
    };

    # Reduce ML thread usage
    machine-learning.environment = {
      MACHINE_LEARNING_REQUEST_THREADS = "1";
      MACHINE_LEARNING_MODEL_INTRA_OP_THREADS = "1";
    };
  };

  systemd.services.immich-server.serviceConfig = {
    SupplementaryGroups = serviceCfg.oidc.systemd.supplementaryGroups;
    MemoryMax = "4G";
    MemoryHigh = "3G";
  };
  systemd.services.immich-machine-learning.serviceConfig = {
    MemoryMax = "5G";
    MemoryHigh = "4G";
  };
}
