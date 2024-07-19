{ pkgs, config, ... }:

{
  services = {
    xserver.enable = true;    # X11 because setting up Wayland is more complicated than it is worth for me.
    desktopManager.plasma6.enable = true;
    displayManager = {
      autoLogin.enable = true;
      autoLogin.user = config.user.name;
      sddm.enable = true;
      defaultSession = "plasmax11";
    };
  };

  environment.plasma6.excludePackages = with pkgs.libsForQt5; [ elisa ];
}
