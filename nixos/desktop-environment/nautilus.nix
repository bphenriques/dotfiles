{ pkgs, lib, network-devices, ... }:
{
  # TODO: https://github.com/ners/trilby/blob/7dd41d0704ebf75f8f705da066184f5ed6168441/modules/home/dconf.nix#L44
  environment.systemPackages = with pkgs; [ pkgs.nautilus ];
  services.gnome.sushi.enable = true;    # Nautilus: previews
  services.gvfs.enable = true;           # Nautilus: Mount, trash, and other functionalities
}