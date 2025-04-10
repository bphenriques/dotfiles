{ config, self, ... }:
let
  mkIcon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; };
in
{
  imports = [
    ../../../config/home-manager
    ../../../config/home-manager/desktop-environment
    ../../../config/home-manager/desktop
    ./kanshi.nix
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
    # Create default directories
    "d ${config.xdg.userDirs.desktop}                         - - - -"
    "d ${config.xdg.userDirs.download}                        - - - -"
    "d ${config.xdg.userDirs.extraConfig.XDG_SCREENSHOTS_DIR} - - - -"
    "d ${config.xdg.userDirs.extraConfig.XDG_RECORDINGS_DIR}  - - - -"

    # Tidy up links to my NAS server under my $HOME
    "L ${config.home.homeDirectory}/nas-private               - - - - /mnt/nas-bphenriques"
    "L ${config.xdg.userDirs.pictures}                        - - - - /mnt/nas-bphenriques/photos"
    "L ${config.home.homeDirectory}/nas-media                 - - - - /mnt/nas-media"
    "L ${config.xdg.userDirs.music}                           - - - - /mnt/nas-media/music"
  ];

  gtk.gtk3.bookmarks = [
    "file://${config.xdg.userDirs.extraConfig.XDG_SCREENSHOTS_DIR}"
    "file://${config.xdg.userDirs.extraConfig.XDG_RECORDINGS_DIR}"
    "file://${config.home.homeDirectory}/nas-private"
    "file://${config.home.homeDirectory}/nas-media"
    "file://${config.home.homeDirectory}/games"
  ];

  custom.programs.screenshot.directory = config.xdg.userDirs.extraConfig.XDG_SCREENSHOTS_DIR;
  custom.programs.screen-recorder.directory = config.xdg.userDirs.extraConfig.XDG_RECORDINGS_DIR;
  custom.programs.file-explorer = {
    enable = true;
    bookmarks = [
      {
        name = "NAS Private";
        icon = mkIcon "nas-private" "󰉐";
        path = "${config.home.homeDirectory}/nas-private";
      }
      {
        name = "NAS Media";
        icon = mkIcon "nas-media" "󰥠";
        path = "${config.home.homeDirectory}/nas-media";
      }
      {
        name = "Games";
        icon = mkIcon "nas-games" "";
        path = "${config.home.homeDirectory}/games";
      }
    ];
  };

  home.stateVersion = "24.05";
}
