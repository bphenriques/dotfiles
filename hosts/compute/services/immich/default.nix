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
  };

  services.immich = {
    enable = true;
    host = serviceCfg.host;
    port = serviceCfg.port;
    mediaLocation = "/var/lib/immich";
    accelerationDevices = [ "/dev/dri/renderD128" ];

    settings = {
      server.externalDomain = serviceCfg.publicUrl;
      newVersionCheck.enabled = false;

      oauth = {
        enabled = true;
        issuerUrl = oidcCfg.provider.issuerUrl;
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
        accel = "qsv";
        acceptedVideoCodecs = [ "h264" "hevc" ];
        preferredHwDevice = "/dev/dri/renderD128";
      };

      storageTemplate = {
        enabled = true;
        hashVerificationEnabled = true;
        template = "{{y}}/{{y}}-{{MM}}-{{dd}}/{{filename}}";
      };

      # Reduce ML-bound job concurrency to limit memory pressure during batch operations
      job = {
        faceDetection.concurrency = 1;
        smartSearch.concurrency = 1;
      };

      # Spread nightly work to avoid a midnight thundering herd
      library.scan.cronExpression = "0 3 * * *";
      nightlyTasks.startTime = "02:00";
    };

    # Reduce ML thread usage
    machine-learning.environment = {
      MACHINE_LEARNING_REQUEST_THREADS = "2";
      MACHINE_LEARNING_MODEL_INTRA_OP_THREADS = "1";
    };
  };

  systemd.services.immich-server.serviceConfig = {
    SupplementaryGroups = serviceCfg.oidc.systemd.supplementaryGroups ++ [ "video" "render" ];
    MemoryMax = "4G";
    MemoryHigh = "3G";
  };
  systemd.services.immich-server.environment.LIBVA_DRIVER_NAME = "iHD"; # Force iHD (intel-media-driver) over legacy i965
  systemd.services.immich-machine-learning.serviceConfig = {
    MemoryMax = "5G";
    MemoryHigh = "4G";
  };
}
