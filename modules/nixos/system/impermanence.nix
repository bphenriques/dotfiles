{ pkgs, lib, config, ... }:

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
  persistStorageConfig = lib.types.submodule {
    options = {
      location = lib.mkOption {
        type = with lib.types; str;
        description = "Location of the persist directory";
      };
      directories = lib.mkOption {
        type = with lib.types; listOf str;
        default = [ ];
        description = "Home directories to persist";
      };
      files = lib.mkOption {
        type = with lib.types; listOf str;
        default = [ ];
        description = "Home files to persist";
      };
    };
  };
in
{
  options.custom.impermanence = {
    enable = lib.mkEnableOption "Whether to enable impermanence";
    rootBlankSnapshot = lib.mkOption {
      description = "Names of the root snapshot to be rolledback upon boot.";
      type = lib.types.str;
      example = "zroot/system/root@blank";
    };

    root = {
      config = lib.mkOption {
        type = persistStorageConfig;
        default = {};
        description = "Persist options for configuration files not managed through Nix.";
      };

      cache = lib.mkOption {
        type = persistStorageConfig;
        default = {};
        description = "Persist options for cache files that are safe to delete but it is preferable to persist";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    # List directories that will be removed on next boot
    environment.systemPackages = [
      (pkgs.writeScriptBin "zfsdiff" ''
        ${pkgs.doas}/bin/doas zfs diff ${cfg.rootBlankSnapshot} -F | ${pkgs.ripgrep}/bin/rg -e "\+\s+/\s+" | cut -f3- | ${pkgs.skim}/bin/sk --query "/home/bphenriques/"
      '')
    ];

    boot.initrd.postDeviceCommands = ''zfs rollback -r ${cfg.rootBlankSnapshot}'';
    security.sudo.extraConfig = "Defaults lecture=never";  # Rollback triggers the lecture everytime

    fileSystems = {
      "${cfg.root.config.location}".neededForBoot = true;
      "${cfg.root.cache.location}".neededForBoot = true;
      "${hmUsersCfg.bphenriques.custom.impermanence.config.location}".neededForBoot = true;
      "${hmUsersCfg.bphenriques.custom.impermanence.cache.location}".neededForBoot = true;
    };

    environment.persistence = let
      merge = lib.foldr (a: b: a // b) { };
      mkPersistDir = { location, directories, files }: {
        "${location}" = {
          inherit files directories;
          hideMounts = true;
        };
      };
      mkHomePersistDir = username: { location, directories, files }: {
        "${location}" = {
          users.${username} = { inherit files directories; };
          hideMounts = true;
        };
      };
    in merge [
      (mkPersistDir cfg.root.config)
      (mkPersistDir cfg.root.cache)
      (mkHomePersistDir "bphenriques" hmUsersCfg.bphenriques.custom.impermanence.config)
      (mkHomePersistDir "bphenriques" hmUsersCfg.bphenriques.custom.impermanence.cache)
    ];
  };
}
