{ pkgs, lib, config, ... }:
{
  environment.systemPackages = with pkgs; [
    vlc
  ];

  services.plex = {
    # Accessible through: http://127.0.0.1:32400/web
    enable = true;
    openFirewall = true;
    # This leads to buggy shutdowns. Disable with sudo systemctl stop plex.service
  };

  # Consider evince as a PDF reader as opposed to firefox
}
