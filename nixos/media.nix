{ pkgs, lib, config, ... }:
{
  # TODO:
  # Take a look at https://git.belanyi.fr/ambroisie/nix-config/src/branch/main/home

  # Music
  services = {
    xserver.desktopManager.plasma5.excludePackages = with pkgs.libsForQt5; [ elisa ]; # Using other music app.
    navidrome = { # Music server
      enable = true; # localhost:4533
      settings = {
        MusicFolder = "/mnt/data/Media/Dropbox/Library/";   # More settings for Subsonic: https://www.navidrome.org/docs/usage/configuration-options/
      };
    };
    plex = {  # Video server
      enable = true;          # Accessible through: http://127.0.0.1:32400/web.
      openFirewall = true;    # So that the TV can connect to the server.
    };

    qbittorrent.enable = true;
  };

  modules.services.jellyfin.enable = true;

  environment.systemPackages = with pkgs; [
    sonixd    # Music Client
    vlc       # Video
  ];
}
