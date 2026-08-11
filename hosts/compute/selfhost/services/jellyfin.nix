{
  config,
  lib,
  pkgs,
  ...
}:
let
  shares = config.custom.shares;
  selfhostMounts = config.selfhost.storage.mounts.smb.shares;

  trickplayScan = {
    EnableRealtimeMonitor = true;
    ExtractTrickplayImagesDuringLibraryScan = true;
    EnableChapterImageExtraction = true;
  };
in
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

      # Only fields Jellyfin actually has. EnableHwAcceleration/EnableHwEncoding/EnableHwDecoding were
      # never real EncodingOptions properties and were silently dropped.
      encoding = {
        HardwareAccelerationType = "qsv";

        # N150 (Alder Lake-N) only has the low-power fixed-function encoder. These must be enabled.
        EnableIntelLowPowerH264HwEncoder = true;
        EnableIntelLowPowerHevcHwEncoder = true;

        # HDR -> SDR tonemapping via OpenCL (requires intel-compute-runtime in hardware.graphics.extraPackages)
        EnableTonemapping = true;
        TonemappingAlgorithm = "bt2390";
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

  users.users.jellyfin.extraGroups = [
    selfhostMounts.media.group
    "video"
    "render"
  ];

  systemd.services.jellyfin = {
    environment.LIBVA_DRIVER_NAME = "iHD"; # Force iHD (intel-media-driver) over legacy i965
    serviceConfig.ReadOnlyPaths = [ "${shares.media.root}/music/library" ];
    serviceConfig.Slice = "throttled.slice";
  };
}
