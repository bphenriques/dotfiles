{ pkgs, lib, config, ... }:
{
  services.qbittorrent.enable = true;

  modules.programs.navidrome.enable = true;

  # Take a look at https://git.belanyi.fr/ambroisie/nix-config/src/branch/main/home

  services.jellyfin = {
    enable = true;
    openFirewall = true;
  };
  environment.systemPackages = with pkgs; [
    jellyfin-ffmpeg
    # or https://github.com/tsirysndr/music-player ?
    # Kid3 for tagging?
    amberol   # Audio
    vlc       # Video
  ];

  services.plex = {
    # Accessible through: http://127.0.0.1:32400/web
    enable = true;
    openFirewall = true;
    # This leads to long shutdowns. Disable with sudo systemctl stop plex.service
  };

  # Consider evince as a PDF reader as opposed to firefox
}
