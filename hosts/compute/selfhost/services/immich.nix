{ config, lib, ... }:
let
  selfhostMounts = config.selfhost.storage.mounts.smb.shares;

  # Everyone with a personal share. Declaring libraries is inert until the user enables Immich, so this
  # needs no opt-in check, and it cannot read config.selfhost.users without recursing on it.
  photoUsers = lib.attrNames (lib.filterAttrs (_: s: s.personal) config.custom.shares);

  # Names are the reconcile identity: renaming one creates a second library.
  mkLibraries =
    user:
    let
      root = config.custom.shares.${user}.root;
    in
    [
      {
        name = "${user}-library";
        importPaths = [ "${root}/photos/library" ];
      }
      {
        name = "${user}-inbox";
        importPaths = [ "${root}/photos/inbox" ];
      }
    ];

  throttled =
    { max, high }:
    {
      Slice = lib.mkForce "throttled.slice";
      MemoryMax = max;
      MemoryHigh = high;
    };

  # No iGPU: it is reserved for Jellyfin and cannot be throttled, so sharing it cooks a small passive box.
  serverBudget = {
    services.immich.settings = {
      ffmpeg = {
        accel = "disabled";
        accelDecode = false;
        acceptedVideoCodecs = [
          "h264"
          "hevc"
        ];
        preset = "veryfast"; # measured on this box: Immich's ultrafast default emits ~30Mbit/s 720p, veryfast is 4x smaller for 7% more CPU
        threads = 2;
      };

      job = {
        videoConversion.concurrency = 1;
        thumbnailGeneration.concurrency = 1;
      };
    };

    systemd.services.immich-server.serviceConfig = throttled {
      max = "4G";
      high = "3G";
    };
  };

  # The memory-hungry half. Single-threaded to stay inside its cap rather than be OOM-killed mid-job.
  mlBudget = {
    services.immich.settings.job = {
      faceDetection.concurrency = 1;
      smartSearch.concurrency = 1;
    };

    services.immich.machine-learning.environment = {
      MACHINE_LEARNING_REQUEST_THREADS = "1";
      MACHINE_LEARNING_MODEL_INTRA_OP_THREADS = "1";
    };

    systemd.services.immich-machine-learning.serviceConfig = throttled {
      max = "5G";
      high = "4G";
    };
  };

  # Keep the heavy scans off the hours anyone is using the box.
  offPeakSchedule = {
    services.immich.settings = {
      library.scan.cronExpression = "0 3 * * *";
      nightlyTasks.startTime = "02:00";
    };
  };
in
lib.mkMerge [
  serverBudget
  mlBudget
  offPeakSchedule

  {
    selfhost = {
      apps.immich.enable = true;

      services.immich = {
        subdomain = "photos";
        access.allowedGroups = [ config.selfhost.groups.admin ];
        extraConfig.landingPage.enable = true;
        systemdServices = [ "immich-server" ];
        storage.mounts = photoUsers;
      };

      users = lib.genAttrs photoUsers (user: { services.immich.libraries = mkLibraries user; });
    };

    services.immich.settings = {
      passwordLogin.enabled = true; # TODO: review whether this is still needed after OIDC is fully rolled out
      library.watch.enabled = false; # inotify doesn't fire on the CIFS-mounted library; the nightly library.scan covers it

      storageTemplate = {
        enabled = true;
        hashVerificationEnabled = true;
        template = "{{y}}/{{y}}-{{MM}}-{{dd}}/{{filename}}";
      };
    };

    users.users.immich.extraGroups = map (user: selfhostMounts.${user}.group) photoUsers;
  }
]
