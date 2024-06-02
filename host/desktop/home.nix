{ config, pkgs, lib, ... }:
{
  # This is an alias (see custom module)
  home = {
    imports = [
      ../../home/config
      ../../home/config/beets
      ../../home/config/sunshine
    ];

    #TODO: Set custom SOPS_AGE_KEY_FILE
    programs.firefox.profiles.default.bookmarks = import ./secrets/bookmarks.age.nix;
    programs.beets.settings.directory = config.user.musicDir;

    # Gpg
    programs.gpg.enable = true;
    services.gpg-agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-gnome3;
    };

    home.packages = with pkgs; [
      killall     # Useful
      discord     # Social
      rofi        # Launcher
    ];

    home.stateVersion = "22.11";
  };
}
