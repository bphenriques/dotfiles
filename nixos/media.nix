{ pkgs, lib, config, ... }:
{
  # TODO:
  # Take a look at https://git.belanyi.fr/ambroisie/nix-config/src/branch/main/home

  # Music
  services = {
    xserver.desktopManager.plasma5.excludePackages = with pkgs.libsForQt5; [ elisa ]; # Using other music app.

    # How to convert to AAC:
    # find . -name "*.mp3" -print0 | parallel -0 ffmpeg -i "{}" -map 0:0 -c:a libfdk_aac -vbr 5 -map 0:1 -c:v copy "{1.}".m4a
    navidrome = { # Music server
      enable = true; # localhost:4533
      settings = {
        MusicFolder = config.user.musicDir;   # More settings for Subsonic: https://www.navidrome.org/docs/usage/configuration-options/
        ScanSchedule = "0";
        Address = "0.0.0.0";  # TIL: 127.0.0.1 is only available in my pc, while 0.0.0.0 allows anything in my network. It is fine because I am behind router and I havent set up port forwarding.
        Port = 4533;
      };
    };
    plex = {  # Video server
      enable = true;          # Accessible through: http://127.0.0.1:32400/web.
      openFirewall = true;    # So that the TV can connect to the server.
    };

    qbittorrent.enable = true;
  };

  networking.firewall = {
    allowedTCPPorts = [ 4533 ]; # Navidrome
  };

  modules.services.jellyfin.enable = true;

  environment.systemPackages = with pkgs; [
    ffmpeg
    sonixd    # Music Client
    vlc       # Video
  ];
}
