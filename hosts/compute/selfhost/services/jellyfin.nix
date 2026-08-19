{ config, lib, pkgs, ... }:
let
  shares = config.custom.shares;

  transcodeDir = "${config.services.jellyfin.cacheDir}/transcodes";

  trickplayScan = {
    EnableRealtimeMonitor = true;
    ExtractTrickplayImagesDuringLibraryScan = true;
    EnableChapterImageExtraction = true;
  };

  # Both blocks below feed `encoding`; names must be real EncodingOptions fields or Jellyfin drops them.

  # Transcode scratch is RAM, not disk. The bounds below exist because of that fixed budget: unbounded,
  # ffmpeg races ~6x ahead and never reclaims, so a long film fills the tmpfs and wedges mid-playback.
  inMemoryTranscoding = {
    selfhost.apps.jellyfin.encoding = {
      EnableThrottling = true;
      ThrottleDelaySeconds = 180;
      EnableSegmentDeletion = true;
      SegmentKeepSeconds = 300;
    };

    # Use spare RAM to transcode segments rather than wearing the SSD. Sized to avoid zram compression.
    fileSystems."${transcodeDir}" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "size=8G" "mode=0700" "nosuid" "nodev" "noexec" ];
    };

    # The mount lands root-owned and jellyfin's uid is allocated at activation, so fix it up by name.
    systemd.tmpfiles.settings.jellyfinTranscodes."${transcodeDir}".d = {
      mode = "700";
      inherit (config.services.jellyfin) user group;
    };
  };

  # QSV on the N150 iGPU: encoder options, device access and driver have to move together.
  qsvTranscoding = {
    selfhost.apps.jellyfin.encoding = {
      HardwareAccelerationType = "qsv";

      # N150 (Alder Lake-N) only has the low-power fixed-function encoder. These must be enabled.
      EnableIntelLowPowerH264HwEncoder = true;
      EnableIntelLowPowerHevcHwEncoder = true;

      # HDR -> SDR tonemapping via OpenCL (requires intel-compute-runtime in hardware.graphics.extraPackages)
      EnableTonemapping = true;
      TonemappingAlgorithm = "bt2390";
    };

    users.users.jellyfin.extraGroups = [ "video" "render" ];
    systemd.services.jellyfin.environment.LIBVA_DRIVER_NAME = "iHD"; # Force iHD (intel-media-driver) over legacy i965
  };
in
lib.mkMerge [
  inMemoryTranscoding
  qsvTranscoding

  {
    selfhost = {
      apps.jellyfin = {
        enable = true;

        libraries = [
          {
            name = "Movies";
            collectionType = "movies";
            locations = [ "${shares.media.root}/movies" ];
            options = trickplayScan;
          }
          {
            name = "TV Shows";
            collectionType = "tvshows";
            locations = [ "${shares.media.root}/tv" ];
            options = trickplayScan;
          }
          {
            name = "Music";
            collectionType = "music";
            locations = [ "${shares.media.root}/music/library" ];
            options = {
              EnableRealtimeMonitor = true;
              ExtractTrickplayImagesDuringLibraryScan = false;
              EnableChapterImageExtraction = false;
            };
          }
        ];

        defaultPolicy = {
          IsHidden = false;
          EnableSubtitleManagement = true;
        };

        branding = {
          SplashscreenEnabled = false;
          CustomCss = builtins.readFile pkgs.elegantfin-jellyfin-theme;
        };

        trickplay = {
          EnableHwAcceleration = true;
          EnableHwEncoding = true;
          ScanBehavior = "NonBlocking";
          ProcessPriority = "BelowNormal";
        };
      };

      services.jellyfin = {
        storage.mounts = [ "media" ];
        extraConfig.landingPage.enable = true;
      };
    };

    users.users.jellyfin.extraGroups = [ config.selfhost.storage.mounts.smb.shares.media.group ];

    systemd.services.jellyfin = {
      serviceConfig.ReadOnlyPaths = [ "${shares.media.root}/music/library" ];
      serviceConfig.Slice = "throttled.slice";
    };
  }
]
