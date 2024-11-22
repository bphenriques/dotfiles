{ pkgs, ... }:
{
  environment.systemPackages = [
    pkgs.nautilus     # File Browser
    pkgs.gnome.sushi  # thumbnails in nautilus
  ];

  services.gvfs.enable = true;          # Mount, trash, and other functionalities
}
