{ lib, pkgs, ... }:

# https://github.com/jordanisaacs/dotfiles/blob/master/modules/system/impermanence/default.nix
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

let
  rootBlankSnapshot = "zroot/system/root@blank";
in
{
  # List directories that will be removed on next boot
  environment.systemPackages = [
    (pkgs.writeScriptBin "zfsdiff" ''
      ${pkgs.doas}/bin/doas zfs diff ${rootBlankSnapshot} -F | ${pkgs.ripgrep}/bin/rg -e "\+\s+/\s+" | cut -f3- | ${pkgs.skim}/bin/sk --query "/home/bphenriques/"
    '')
  ];

  environment.persistence = {
    "/persist/config/system" = {
      hideMounts = true;
      directories = [
        "/var/log"

        # Docker
        "/var/lib/docker"

        # Connectivity
        "/var/lib/bluetooth"
        "/var/lib/nixos" # https://github.com/nix-community/impermanence/issues/178
        "/etc/NetworkManager"
      ];
      files = [
        "/etc/machine-id"
      ];
    };

    "/persist/cache/system" = {
      hideMounts = true;
      files = [ ];
      directories = [ ];
    };

    "/persist/config/bphenriques" = {
      hideMounts = true;
      users.bphenriques = {
        directories = [
          "Downloads"
          "Music"
          "Pictures"
          "Videos"
          ".config/systemd" # git maintenance systemd timers
          ".config/vlc"
          ".mozilla"        # Firefox
          ".config/sops"

          ".dotfiles"

          # SSH
          { directory = ".ssh"; mode = "0700"; }

          # Steam
          ".local/share/Steam"
          ".config/lutris"

          ".local/share/nix" # trusted settings and repl history
        ];
        files = [ ];
      };
    };

    "/persist/cache/bphenriques" = {
      hideMounts = true;
      users.bphenriques = {
        directories = [
          ".cache/dconf"
          ".config/dconf"
          ".cache/nix"
          ".cache/mozilla"  # Firefox

          ".local/share/lutris"

          ".config/sunshine"

          # Shell
          ".local/share/fish"
          ".local/share/zoxide"
          ".bash_history"
        ];
        files = [ ];
      };
    };
  };

  fileSystems = {
    "/persist/cache/bphenriques".neededForBoot = true;
    "/persist/cache/system".neededForBoot = true;
    "/persist/config/bphenriques".neededForBoot = true;
    "/persist/config/system".neededForBoot = true;
  };

  boot.initrd.postDeviceCommands = lib.mkAfter ''zfs rollback -r ${rootBlankSnapshot};'';
  security.sudo.extraConfig = "Defaults lecture=never";  # Rollback triggers the lecture everytime
}


