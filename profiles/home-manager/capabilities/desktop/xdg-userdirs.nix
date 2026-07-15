{ config, lib, pkgs, ... }:
{
  xdg.userDirs = {
    enable = true;
    createDirectories = false;  # Created separately via tmpfiles
    setSessionVariables = true;

    desktop   = "${config.home.homeDirectory}/desktop";
    pictures  = "${config.home.homeDirectory}/pictures";
    documents = "${config.home.homeDirectory}/workdir";
    music     = "${config.home.homeDirectory}/music";
    download  = "${config.home.homeDirectory}/downloads";

    extraConfig.SCREENSHOTS = "${config.home.homeDirectory}/screenshots";
    extraConfig.RECORDINGS = "${config.home.homeDirectory}/recordings";
  };

  # https://www.mankier.com/5/tmpfiles.d
  systemd.user.tmpfiles.rules = lib.optionals pkgs.stdenv.isLinux [
    "d ${config.xdg.userDirs.desktop}     - - - -"
    "d ${config.xdg.userDirs.pictures}    - - - -"
    "d ${config.xdg.userDirs.music}       - - - -"
    "d ${config.xdg.userDirs.download}    - - - -"
    "d ${config.xdg.userDirs.extraConfig.SCREENSHOTS} - - - -"
    "d ${config.xdg.userDirs.extraConfig.RECORDINGS}  - - - -"
  ];

  custom.programs.screenshot.directory = config.xdg.userDirs.extraConfig.SCREENSHOTS;
  custom.programs.screen-recorder.directory = config.xdg.userDirs.extraConfig.RECORDINGS;

  gtk.gtk3.bookmarks = [
    "file://${config.xdg.userDirs.extraConfig.SCREENSHOTS}"
    "file://${config.xdg.userDirs.extraConfig.RECORDINGS}"
  ];
}
