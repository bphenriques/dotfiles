{ config, self, ... }:
let
  mkIcon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; };

  mounts = {
    nasPrivate = "/mnt/nas-bphenriques";
    nasMedia = "/mnt/nas-media";
  };
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

    desktop   = "${config.home.homeDirectory}/desktop";
    pictures  = "${config.home.homeDirectory}/pictures";
    documents = "${config.home.homeDirectory}/workdir";  # btrfs mount
    music     = "${config.home.homeDirectory}/music";
    download  = "${config.home.homeDirectory}/downloads";

    extraConfig.XDG_SCREENSHOTS_DIR = "${config.home.homeDirectory}/screenshots"; # Non standard used by some apps.
    extraConfig.XDG_RECORDINGS_DIR = "${config.home.homeDirectory}/recordings";
  };

  # https://www.mankier.com/5/tmpfiles.d
  systemd.user.tmpfiles.rules = [
    # Create default directories
    "d ${config.xdg.userDirs.desktop}                         - - - -"
    "d ${config.xdg.userDirs.pictures}                        - - - -"
    "d ${config.xdg.userDirs.music}                           - - - -"
    "d ${config.xdg.userDirs.download}                        - - - -"
    "d ${config.xdg.userDirs.extraConfig.XDG_SCREENSHOTS_DIR} - - - -"
    "d ${config.xdg.userDirs.extraConfig.XDG_RECORDINGS_DIR}  - - - -"

    # Note: avoiding mounting directy to avoid slowing down access to $HOME in-case I am offline.
    "L ${config.xdg.userDirs.pictures}/nas                    - - - - ${mounts.nasPrivate}/photos"
    "L ${config.xdg.userDirs.music}/nas                       - - - - ${mounts.nasMedia}/music"
  ];

  gtk.gtk3.bookmarks = [
    "file://${config.xdg.userDirs.extraConfig.XDG_SCREENSHOTS_DIR}"
    "file://${config.xdg.userDirs.extraConfig.XDG_RECORDINGS_DIR}"

    # Samba mounts
    "file://${mounts.nasPrivate}"
    "file://${mounts.nasMedia}"
  ];

  custom.programs.screenshot.directory = config.xdg.userDirs.extraConfig.XDG_SCREENSHOTS_DIR;
  custom.programs.screen-recorder.directory = config.xdg.userDirs.extraConfig.XDG_RECORDINGS_DIR;
  custom.programs.file-explorer = {
    enable = true;
    bookmarks = [
      {
        name = "NAS Private";
        icon = mkIcon "nas-private" "󰉐";
        path = mounts.nasPrivate;
      }
      {
        name = "NAS Media";
        icon = mkIcon "nas-media" "󰥠";
        path = mounts.nasMedia;
      }
    ];
  };

  home.stateVersion = "24.05";
}
