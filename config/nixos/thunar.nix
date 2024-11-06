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

  programs.xfconf.enable = true;       # Required to persist Thunar settings as we're not running on XFCE
  services.gvfs.enable = true;          # Mount, trash, and other functionalities
  services.tumbler.enable = true;       # Thumbnail support for images
  programs.file-roller.enable = true;   # For thunar-archive-plugin
}