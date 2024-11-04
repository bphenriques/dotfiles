{ pkgs, ... }:
{
  # https://wiki.archlinux.org/title/Thunar
  programs.thunar = {
    enable = true;
    plugins = with pkgs.xfce; [
		  exo
		  thunar-archive-plugin # Unarchive filess more easily
		  thunar-volman         # Removable media
    ];
  };

  services.gvfs.enable = true;    # Mount, trash, and other functionalities
  services.tumbler.enable = true; # Thumbnail support for images
}