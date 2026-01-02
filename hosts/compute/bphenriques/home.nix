{ config, self, ... }:
let
  mounts = {
    nasPrivate = "/mnt/nas-bphenriques";
    nasMedia = "/mnt/nas-media";
  };
in
{
  imports = [
    ../../../config/home-manager
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

    # Note: avoiding mounting directly to avoid slowing down access to $HOME in-case I am offline.
    "L ${config.xdg.userDirs.pictures}/nas                    - - - - ${mounts.nasPrivate}/photos"
    "L ${config.xdg.userDirs.music}/nas                       - - - - ${mounts.nasMedia}/music"
  ];

  home.stateVersion = "24.05";
}
