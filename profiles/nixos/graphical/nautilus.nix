{ pkgs, ... }:
{
  environment.systemPackages = [ pkgs.nautilus ];
  services.gnome.sushi.enable = true; # Previews
  services.gvfs.enable = true;        # Mount, trash, and other functionalities
}