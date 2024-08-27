{ pkgs, lib, config, ... }:

let
  cfg = config.custom.impermanence;
  mkImpermanenceOption = default: lib.mkOption {
    inherit default;
    type = lib.types.bool;
  };
in
{
  options.custom.impermanence = {
    enable = lib.mkEnableOption "nix-os-impermanence";
    rootBlankSnapshot = lib.mkOption {
      description = "Names of the root snapshot to be rolledback upon boot.";
      type = lib.types.str;
      example = "zroot/system/root@blank";
    };

    dataLocation = lib.mkOption {
      type = with lib.types; str;
      description = "Location of the system's configuration persist directory";
    };

    cacheLocation = lib.mkOption {
      type = with lib.types; str;
      description = "Location of the system's configuration persist directory";
    };

    # Security
    fprintd = mkImpermanenceOption config.services.fprintd.enable;

    # Networking
    networkmanager = mkImpermanenceOption config.networking.networkmanager.enable;
    bluetooth = mkImpermanenceOption config.hardware.bluetooth.enable;

    # Other
    docker = mkImpermanenceOption config.virtualisation.docker.enable;
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
         assertion = !config.users.mutableUsers;
         message = "users.mutableUsers must be false, otherwise issues appear. See https://github.com/nix-community/impermanence/issues/120.";
      }
    ];

    fileSystems = {
      "${cfg.dataLocation}".neededForBoot = true;
      "${cfg.cacheLocation}".neededForBoot = true;
    };

    boot.initrd.postDeviceCommands = lib.mkAfter ''zfs rollback -r ${cfg.rootBlankSnapshot};'';
    programs.fuse.userAllowOther = true; # Allows users to specify the "allowOther" option.
    environment.persistence = {
      "${cfg.dataLocation}" = {
        hideMounts = true;
        directories = [
          "/var/log"
          "/var/lib/nixos" # https://github.com/nix-community/impermanence/issues/178
        ]
          ++ lib.optionals cfg.networkmanager [ "/etc/NetworkManager" ]
          ++ lib.optionals cfg.bluetooth      [ "/var/lib/bluetooth" ]
          ++ lib.optionals cfg.docker         [ "/var/lib/docker" ]
          ++ lib.optionals cfg.fprintd        [ "/var/lib/fprint" ];

        files = [ ]
          ++ lib.optionals cfg.networkmanager [ "/etc/machine-id" ];
      };

      "${cfg.cacheLocation}" = {
        hideMounts = true;
        directories = [
          "/var/lib/systemd/coredump" # Systemd core-dumps that I might want to store if requested but won't really look at them
          "/var/lib/upower"           # Tracks power since beginning of time
        ];
        files = [ ];
      };
    };
  };
}
