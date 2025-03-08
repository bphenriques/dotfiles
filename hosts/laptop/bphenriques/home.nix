{ pkgs, config, self, ... }:
let
  mkIcon = name: symbol: self.lib.builders.mkNerdFontIcon pkgs { textColor = config.lib.stylix.colors.withHashtag.base07; } name symbol;
in
{
  imports = [
    ../../../home-manager
    ../../../home-manager/desktop-environment
    ../../../home-manager/desktop
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

  custom.programs.screenshot.directory = config.xdg.userDirs.extraConfig.XDG_SCREENSHOTS_DIR;
  custom.programs.screen-recorder.directory = config.xdg.userDirs.extraConfig.XDG_RECORDINGS_DIR;
  custom.programs.volume-osd.defaultSinks = {
    internal = {
      name = "alsa_output.pci-0000_06_00.6.analog-stereo";
      label = "Internal Speaker";
    };
    external = {
      name = "alsa_output.pci-0000_01_00.1.hdmi-stereo";
      label = "External Speaker";
    };
    headphones = {
      name = "alsa_output.usb-SteelSeries_SteelSeries_Arctis_7-00.stereo-game";
      label = "Steel Series Arctis 7 (Game)";
    };
  };

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