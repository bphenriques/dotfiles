{ config, self, ... }:
let
  pathsCfg = config.custom.paths;
in
{
  imports = [
    ../../../config/home-manager
    ./stylix.nix
  ];

  xdg.userDirs = {
    enable = true;
    createDirectories = false;  # Created separately

    desktop   = "${config.home.homeDirectory}/desktop";
    pictures  = "${config.home.homeDirectory}/pictures";
    music     = "${config.home.homeDirectory}/music";
    download  = "${config.home.homeDirectory}/downloads";
  };

  # https://www.mankier.com/5/tmpfiles.d
  systemd.user.tmpfiles.rules = [
    # Create default directories
    "d ${config.xdg.userDirs.desktop}                         - - - -"
    "d ${config.xdg.userDirs.pictures}                        - - - -"
    "d ${config.xdg.userDirs.music}                           - - - -"
    "d ${config.xdg.userDirs.download}                        - - - -"

    # Note: avoiding mounting directly to avoid slowing down access to $HOME when offline.
    "L ${config.xdg.userDirs.pictures}/nas                    - - - - ${pathsCfg.photos.root}"
    "L ${config.xdg.userDirs.music}/nas                       - - - - ${pathsCfg.music.root}"
  ];

  home.stateVersion = "24.05";
}
