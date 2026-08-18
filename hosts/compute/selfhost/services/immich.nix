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
in
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

  services.immich = {
    settings = {
      passwordLogin.enabled = true; # TODO: review whether this is still needed after OIDC is fully rolled out
      library.watch.enabled = false; # inotify doesn't fire on the CIFS-mounted library; the nightly library.scan covers it

      ffmpeg = {
        accel = "disabled"; # CPU-only; iGPU reserved for Jellyfin and I can't throttle GPU. Both will lead to thermal issues in a small device.
        accelDecode = false;
        acceptedVideoCodecs = [
          "h264"
          "hevc"
        ];
        preset = "veryfast"; # measured on this box: Immich's ultrafast default emits ~30Mbit/s 720p, veryfast is 4x smaller for 7% more CPU
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

  users.users.immich.extraGroups = map (user: selfhostMounts.${user}.group) photoUsers;

  systemd.services.immich-server.serviceConfig = {
    Slice = lib.mkForce "throttled.slice";
    MemoryMax = "4G";
    MemoryHigh = "3G";
  };

  systemd.services.immich-machine-learning.serviceConfig = {
    Slice = lib.mkForce "throttled.slice";
    MemoryMax = "5G";
    MemoryHigh = "4G";
  };
}
