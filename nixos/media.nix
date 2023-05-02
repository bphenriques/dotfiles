{ pkgs, lib, config, ... }:
{
  # TODO:
  # Take a look at https://git.belanyi.fr/ambroisie/nix-config/src/branch/main/home

  # Music
  environment = {
    plasma5.excludePackages = with pkgs.libsForQt5; [ elisa ]; # Using other music app.
    systemPackages = with pkgs; [
      museeks   # Audio
      vlc       # Video
    ];
  };

  modules.services.jellyfin.enable = true;
  services.plex = {
    enable = true;          # Accessible through: http://127.0.0.1:32400/web.
    openFirewall = true;    # So that the TV can connect to the server.
  };
}

