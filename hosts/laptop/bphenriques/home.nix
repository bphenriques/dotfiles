{ pkgs, config, self, ... }:
{
  imports = [
    ../../../config/home-manager
    ../../../config/home-manager/gaming
    ../../../config/home-manager/window-manager
    ./input-remapper
  ];

  xdg.userDirs = {
    enable = true;
    createDirectories = false;

    documents = "${config.home.homeDirectory}/workdir";
    pictures  = "${config.home.homeDirectory}/pictures";
    music     = "${config.home.homeDirectory}/music";
    desktop   = "${config.home.homeDirectory}/desktop";
    download  = "${config.home.homeDirectory}/downloads";

    extraConfig.XDG_SCREENSHOTS_DIR = "${config.home.homeDirectory}/screenshots"; # Non standard used by some apps.
  };

  # https://www.mankier.com/5/tmpfiles.d
  systemd.user.tmpfiles.rules = [
    # Tidy up most things under $HOME
    "L ${config.home.homeDirectory}/games                     - - - - /mnt/games"
    "L ${config.xdg.userDirs.documents}                       - - - - /mnt/bphenriques"
    "L ${config.xdg.userDirs.pictures}                        - - - - /mnt/nas-bphenriques/photos"
    "L ${config.xdg.userDirs.music}                           - - - - /mnt/nas-media/music"
    "d ${config.xdg.userDirs.desktop}                         - - - -"
    "d ${config.xdg.userDirs.download}                        - - - -"
    "d ${config.xdg.userDirs.extraConfig.XDG_SCREENSHOTS_DIR} - - - -"
  ];

  gtk.gtk3.bookmarks = [
    "file://${config.xdg.userDirs.documents}"
    "file://${config.xdg.userDirs.pictures}"
    "file://${config.xdg.userDirs.music}"
    "file://${config.xdg.userDirs.desktop}"
    "file://${config.xdg.userDirs.download}"
    "file://${config.xdg.userDirs.extraConfig.XDG_SCREENSHOTS_DIR}"

    # Other
    "file://${config.home.homeDirectory}/games"
    "file://${config.home.homeDirectory}/.config Config"
  ];

  home.stateVersion = "24.05";
}
