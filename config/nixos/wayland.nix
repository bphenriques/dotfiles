{ pkgs, ... }:
{
  # https://wiki.hyprland.org/Nvidia/
  #environment.sessionVariables = {
  #  LIBVA_DRIVER_NAME = "nvidia";
  #  XDG_SESSION_TYPE = "wayland";
  #  GBM_BACKEND = "nvidia-drm";
  #  __GLX_VENDOR_LIBRARY_NAME = "nvidia";
  #};

  # TODO: Can I run this on demand?
  programs.nm-applet.enable = true;

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
      xdg-desktop-portal-hyprland
    ];
  };

  programs.hyprland.enable = true;  # Home-Manager sets the rest
}