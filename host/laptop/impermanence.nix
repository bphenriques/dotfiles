{ lib, ... }:
{
  custom.impermanence = {
    enable = false;
    rootBlankSnapshot = "zroot/system/root@blank";
    root = {
      config = {
        location = "/persist/config/system";
        directories = [
          "/var/log"
          "/var/lib/nixos" # https://github.com/nix-community/impermanence/issues/178

          # Connectivity
          "/var/lib/bluetooth"
          "/etc/NetworkManager"
        ];
        files = [ "/etc/machine-id" ];
      };
      cache.location = "/persist/cache/system";
    };
  };

  home-manager.users.bphenriques.custom.impermanence = {
    config = {
      location = "/persist/config/bphenriques";
      directories = [
        "Downloads"
        "Music"
        "Pictures"
        "Videos"
        ".config/vlc"
        ".config/sops"

        ".dotfiles"
        ".ssh"

        # Steam
        ".local/share/Steam"
        ".config/lutris"

        ".local/share/nix" # trusted settings and repl history
      ];
      files = [];
    };
    cache = {
      location = "/persist/cache/bphenriques";
      directories = [
        ".cache/dconf"
        ".config/dconf"
        ".cache/nix"

        ".local/share/lutris"

        ".config/sunshine"
        ".config/sops"

        # Shell
        ".local/share/fish"
        ".local/share/zoxide"
        ".bash_history"
      ];
      files = [];
    };
  };
}




