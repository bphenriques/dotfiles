{ pkgs, lib, config, ... }:

# NOT BEING USED. Iterating...

# Imper bla bla : https://github.com/iynaix/dotfiles/blob/main/nixos/impermanence.nix#L59
# https://github.com/search?q=repo%3Athexyno%2Fnixos-config%20ragon.persist&type=code
  # For when I use tmpfs: https://github.com/iynaix/dotfiles/blob/main/nixos/impermanence.nix#L34

# https://gitlab.com/usmcamp0811/dotfiles/-/tree/nixos/modules/nixos/hardware?ref_type=heads
# https://github.com/jordanisaacs/dotfiles/blob/master/modules/system/impermanence/default.nix
# https://github.com/AntonHakansson/nixos-config/blob/main/modules/core/zfs/default.nix#L60
# Imper bla bla : https://github.com/iynaix/dotfiles/blob/main/nixos/impermanence.nix#L59
# This one is nice: https://github.com/iynaix/dotfiles/blob/main/nixos/impermanence.nix#L35
# https://github.com/search?q=repo%3Athexyno%2Fnixos-config%20ragon.persist&type=code
# https://github.com/jordanisaacs/dotfiles/blob/master/modules/system/impermanence/default.nix

# TODO: Consider using custom options: https://github.com/nix-community/impermanence/blob/master/nixos.nix#L128. Annoying but might need
let
  cfg = config.custom.impermanence;
  hmUsersCfg = config.home-manager.users;
in
{
  options.custom.impermanence = {
    enable = lib.mkEnableOption "Whether to enable impermanence";
    rootBlankSnapshot = lib.mkOption {
      description = "Names of the root snapshot to be rolledback upon boot.";
      type = lib.types.str;
      example = "zroot/system/root@blank";
    };

    configLocation = lib.mkOption {
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
        ${pkgs.doas}/bin/doas zfs diff ${cfg.rootBlankSnapshot} -F | ${pkgs.ripgrep}/bin/rg -e "\+\s+/\s+" | cut -f3- | ${pkgs.skim}/bin/sk --query "/home/bphenriques/"
      '')
    ];

    fileSystems = {
      "${cfg.configLocation}".neededForBoot = true;
      "${cfg.cacheLocation}".neededForBoot = true;
      "${hmUsersCfg.bphenriques.custom.impermanence.configLocation}".neededForBoot = true;
      "${hmUsersCfg.bphenriques.custom.impermanence.cacheLocation}".neededForBoot = true;
    };

    boot.initrd.postDeviceCommands = ''zfs rollback -r ${cfg.rootBlankSnapshot};'';
    environment.persistence = {
      "${cfg.configLocation}" = {
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
