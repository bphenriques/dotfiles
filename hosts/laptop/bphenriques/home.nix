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
    extraConfig.XDG_RECORDINGS_DIR = "${config.home.homeDirectory}/recordings";
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
    "d ${config.xdg.userDirs.extraConfig.XDG_RECORDINGS_DIR}  - - - -"
  ];

  gtk.gtk3.bookmarks = [
    "file://${config.xdg.userDirs.extraConfig.XDG_SCREENSHOTS_DIR}"
    "file://${config.home.homeDirectory}/nas-private"
    "file://${config.home.homeDirectory}/nas-media"
    "file://${config.home.homeDirectory}/games"
  ];

  custom.programs.swappy.directory = config.xdg.userDirs.extraConfig.XDG_SCREENSHOTS_DIR;
  custom.programs.shortcuts = {
    enable = true;
    files.bookmarks = [
      {
        name = "NAS Private";
        icon = self.lib.builders.mkNerdFontIcon pkgs "nas-private" "󰉐g";
        path = "${config.home.homeDirectory}/nas-private";
      }
      {
        name = "NAS Media";
        icon = self.lib.builders.mkNerdFontIcon pkgs "nas-media" "󰥠";
        path = "${config.home.homeDirectory}/nas-media";
      }
      {
        name = "Games";
        icon = self.lib.builders.mkNerdFontIcon pkgs "nas-games" "";
        path = "${config.home.homeDirectory}/games";
      }
    ];
  };

  home.stateVersion = "24.05";
}