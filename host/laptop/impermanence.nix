{ ... }:

# Imper bla bla : https://github.com/iynaix/dotfiles/blob/main/nixos/impermanence.nix#L59
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
    config = rec {
      source = "/persist/${user}";
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
      source = "/persist/${user}/cache";
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
}



