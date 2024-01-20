{ config, pkgs, lib, ... }:
{
  # This is an alias (see custom module)
  home = {
    imports = [
      ../../home
      ../../home/config/beets
      ../../home/config/sunshine
    ];

    programs.beets.settings.directory = config.user.musicDir;

    # Gpg
    programs.gpg.enable = true;
    services.gpg-agent = {
      enable = true;
      pinentryFlavor = "gnome3";
    };

    home.packages = with pkgs; [
      killall     # Useful
      discord     # Social
      rofi        # Launcher
    ];

    home.stateVersion = "22.11";
  };
}
