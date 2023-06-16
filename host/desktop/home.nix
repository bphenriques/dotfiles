{ config, pkgs, lib, ... }:
{
  # This is an alias (see custom module)
  home = {
    imports = [
      ../../home
      ../../home/fonts.nix
      ../../home/config/beets
      ../../home/config/sunshine
    ];

    programs.beets.settings = {
      directory = config.user.musicDir;
      library = "/mnt/data/Media/Music/beets-library.db";
    };

    # Programs
    programs.firefox.enable = true;
    services.dropbox.enable = true; # TODO: Change path but ensure that the folders

    # Gpg
    programs.gpg.enable = true;
    services.gpg-agent.enable = true;

    # Social
    modules.programs.discord.enable = true;
    home.packages = with pkgs; [
      rofi        # Launcher
    ];

    home.stateVersion = "22.11";
  };
}
