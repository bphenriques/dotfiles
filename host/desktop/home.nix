{ config, pkgs, lib, ... }:
{
  # This is an alias (see custom module)
  home = {
    imports = [
      ../../home
      ../../home/config/beets
      ../../home/config/sunshine
    ];

    programs.beets.settings = {
      directory = config.user.musicDir;
      library = "/mnt/data/Media/Music/beets-library.db";
    };

    services.dropbox.enable = true; # TODO: Change path but ensure that the folders

    # Gpg
    programs.gpg.enable = true;
    services.gpg-agent.enable = true;

    programs.firefox.profiles.default.containers = {
      "Personal" = {
        id = 1;
        color = "turquoise";
        icon = "chill";
      };
      "Banking" = {
        id = 2;
        color = "red";
        icon = "fingerprint";
      };
      "Shopping" = {
        id = 3;
        color = "orange";
        icon = "cart";
      };
      "Social" = {
        id = 4;
        color = "yellow";
        icon = "tree";
      };
    };

    home.packages = with pkgs; [
      discord     # Social
      rofi        # Launcher
    ];

    home.stateVersion = "22.11";
  };
}
