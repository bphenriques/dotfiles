{ pkgs, config, ... }:

{
  # Display environment
  services.xserver = {
    enable = true;                        # X11 because setting up Wayland is more complicated than it is worth for me.
    desktopManager.plasma5.enable = true; # Plasma environment.

    displayManager = {
        autoLogin.enable = true;
        autoLogin.user = config.user.name;
        sddm.enable = true;    # SDDM login page.
    };
  };

  environment.plasma5.excludePackages = with pkgs.libsForQt5; [ elisa ];
}
