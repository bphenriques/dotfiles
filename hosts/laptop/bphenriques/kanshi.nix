{ pkgs, config, ... }:
let
  laptopScreen = {
    criteria = "eDP-1";
    mode = "2880x1800@120.001Hz";
    scale = 1.75;
  };
  dellScreen = {
    criteria = "Dell Inc. DELL S2721DGF 4P11R83";
    mode = "2560x1440@143.92Hz";
    scale = 1.0;
  };

  enable = screen: screen // { status = "enable"; };
  disable = screen: screen // { status = "disable"; };
in
{
  services.kanshi = {
    enable = true;
    settings = [
       {
         profile.name = "internal";
         profile.outputs = [
           (enable laptopScreen)
         ];
       }
       {
         profile.name = "external-office";
         profile.outputs = [
           (disable laptopScreen)
           (enable dellScreen)
         ];
       }
    ];
  };

  custom.desktop-environment.settings.displayOutput = {
    identifier  = "eDP-1";
    resolution  = "2880x1800";
    refreshRate = "120.001"; # using float leads to trailing zerosthat I dont want
    scale       = "1.75";
  };
}