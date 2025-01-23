{ pkgs, ... }:
{
  environment.systemPackages = [ pkgs.nautilus ];
  services.gnome.sushi.enable = true;    # Nautilus: previews
  services.gvfs.enable = true;           # Nautilus: Mount, trash, and other functionalities
}