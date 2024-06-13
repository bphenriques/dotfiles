{ config, pkgs, lib, ... }:
{
  # This is an alias (see custom module)
  home = {
    imports = [
      #../../home/config
      ../../home/config/plasma
    ];
    programs.plasma.workspace.wallpaper = ./wallpaper.png;

    # Gpg
    programs.gpg.enable = true;
    services.gpg-agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-gnome3;
    };

    home.packages = with pkgs; [
      killall     # Useful
    ];

    home.stateVersion = "24.05";
  };
}
