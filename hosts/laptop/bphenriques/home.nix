{ pkgs, config, self, ... }:
{
  imports = [
    ../../../home-manager
    ../../../home-manager/desktop-environment
    ../../../home-manager/desktop
    ./kanshi.nix
    ./waybar.nix
    ./stylix.nix
  ];

  xdg.userDirs = {
    enable = true;
    createDirectories = false;  # Created separately

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
    "L ${config.home.homeDirectory}/nas-private               - - - - /mnt/nas-bphenriques"
    "L ${config.home.homeDirectory}/nas-media                 - - - - /mnt/nas-media"
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
    "file://${config.home.homeDirectory}/nas-private"
    "file://${config.home.homeDirectory}/nas-media"
    "file://${config.home.homeDirectory}/games"
    "file://${config.home.homeDirectory}/.config Config"
  ];

  custom.desktop-environment.file-bookmarks = [
    { name = "Documents"; path = config.xdg.userDirs.documents; }
    { name = "Pictures"; path = config.xdg.userDirs.pictures; }
    { name = "Music"; path = config.xdg.userDirs.music; }
    { name = "Downloads"; path = config.xdg.userDirs.download; }
    { name = "NAS Private"; path = "${config.home.homeDirectory}/nas-private"; }
    { name = "NAS Media"; path = "${config.home.homeDirectory}/nas-media"; }
    { name = "Games"; path = "${config.home.homeDirectory}/games"; }
  ];

  home.stateVersion = "24.05";
}
