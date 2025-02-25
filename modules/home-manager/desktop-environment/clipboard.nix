{ pkgs, lib, ... }:
{
  config = {
    home.packages = [
      pkgs.wl-clipboard        # Wayland clipboard
    ];
  };
}