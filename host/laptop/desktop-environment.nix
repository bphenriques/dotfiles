{ pkgs, config, ... }:

{
  services = {
    xserver.enable = true;    # X11 because setting up Wayland is more complicated than it is worth for me.
    desktopManager.plasma6.enable = true;
    displayManager = {
      sddm.enable = true;
      defaultSession = "plasmax11";
    };
  };

  # TODO Plasma 6 https://nixos.wiki/wiki/KDE#Plasma_6
  environment.plasma6.excludePackages = with pkgs.libsForQt5; [ elisa ];
}
