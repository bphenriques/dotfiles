{ ... }:

# Imper bla bla : https://github.com/iynaix/dotfiles/blob/main/nixos/impermanence.nix#L59
# https://github.com/search?q=repo%3Athexyno%2Fnixos-config%20ragon.persist&type=code
let
  system = {
    config = {
      source = "/persist/system";
      directories = [
        "/var/log"

        # Connectivity
        "/var/lib/bluetooth"
        "/var/lib/nixos" # https://github.com/nix-community/impermanence/issues/178
        "/etc/NetworkManager/system-connections"
      ];
      files = [
        "/etc/machine-id"
      ];
    };
    cache = {
      source = "/persist/system/cache";
      directories = [];
      files = [];
    };
  };

  bphenriques = {
    username = "bphenriques";
    config = {
      source = "/persist/bphenriques";
      directories = [
        "Downloads"
        ".config/systemd" # git maintenance systemd timers
        ".config/vlc"
        ".mozilla"        # Firefox
        ".config/sops"

        # SSH
        { directory = ".ssh"; mode = "0700"; }

        # Steam
        ".local/share/Steam"
        ".config/lutris"

        ".local/share/nix" # trusted settings and repl history
      ];
      files = [
      ];
    };
    cache = {
      source = "/persist/bphenriques/cache";
      directories = [
        ".cache/dconf"
        ".config/dconf"
        ".cache/nix"
        ".cache/mozilla"  # Firefox

        ".local/share/lutris"

        # Shell
        "local/share/fish"
        ".local/share/zoxide"
        ".bash_history"
      ];
      files = [];
    };
  };
in
{
  environment.persistence = {
    "${system.config.source}" = {
      hideMounts = true;
      files = system.config.files;
      directories = system.config.directories;
    };

    "${system.cache.source}" = {
      hideMounts = true;
      files = system.cache.files;
      directories = system.cache.directories;
    };

    "${bphenriques.config.source}" = {
      hideMounts = true;
      users.${bphenriques.username} = {
        directories = bphenriques.config.directories;
        files = bphenriques.config.files;
      };
    };

    "${bphenriques.cache.source}" = {
      hideMounts = true;
      users.${bphenriques.username} = {
        directories = bphenriques.cache.directories;
        files = bphenriques.cache.files;
      };
    };
  };

  boot.initrd.postDeviceCommands = ''zfs rollback -r zroot/system/root@blank'';
  security.sudo.extraConfig = ''
    # Rollback triggers the lecture everytime
    Defaults lecture = never
  '';
}

# https://github.com/jordanisaacs/dotfiles/blob/master/modules/system/impermanence/default.nix


