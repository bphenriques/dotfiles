{ pkgs, lib, network-devices, ... }:
{
  programs.niri.enable = true;
  #security.pam.services.swaylock = {};
  services.gnome.gnome-keyring.enable = true;
  xdg.portal = {
    enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-gnome # Required for screencasting
    ];
  };
}
