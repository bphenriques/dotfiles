{ pkgs, lib, config, ... }:

{
  # Display environment
  services.xserver = {
    enable = true;                        # X11 because setting up Wayland is more complicated than it is worth for me.
    displayManager.sddm.enable = true;    # SDDM login page.
    desktopManager.plasma5.enable = true; # Plasma environment.
  };

  environment.plasma5.excludePackages = with pkgs.libsForQt5; [ elisa ];
}
