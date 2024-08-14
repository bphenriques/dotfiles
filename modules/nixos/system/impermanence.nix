{ pkgs, lib, config, ... }:

let
  cfg = config.custom.impermanence;
  hmUsersCfg = config.home-manager.users;
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
  };

  config = lib.mkIf cfg.enable {
    # List directories that will be removed on next boot
    environment.systemPackages = [
      (pkgs.writeScriptBin "zfsdiff" ''
        sudo zfs diff ${cfg.rootBlankSnapshot} -F | ${pkgs.ripgrep}/bin/rg -e "\+\s+/\s+" | cut -f3- | ${pkgs.skim}/bin/sk --query "/home/bphenriques/"
      '')
    ];

    fileSystems = {
      "${cfg.dataLocation}".neededForBoot = true;
      "${cfg.cacheLocation}".neededForBoot = true;
    };

    boot.initrd.postDeviceCommands = lib.mkAfter ''zfs rollback -r ${cfg.rootBlankSnapshot};'';
    environment.persistence = {
      "${cfg.dataLocation}" = {
        hideMounts = true;
        directories = [
          "/var/log"
          "/var/lib/bluetooth"
          "/var/lib/nixos" # https://github.com/nix-community/impermanence/issues/178
          "/etc/NetworkManager"
        ];
        files = [
          "/etc/machine-id"
        ];
      };

      "${cfg.cacheLocation}" = {
        hideMounts = true;
        directories = [ ];
        files = [ ];
      };
    };
  };
}
