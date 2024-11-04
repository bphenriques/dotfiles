{ pkgs, config, ... }:
# More examples:
# - https://github.com/colemickens/nixcfg/blob/f28e80177328a7e005bea96d0931a0f1a98d9df5/mixins/kanshi.nix#L21
# - https://github.com/MatthewCroughan/nixcfg/blob/afab322e6da20cc038d8577dd4a365673702d183/users/matthew/modules/kanshi.nix#L2
let
  laptopScreen = {
    criteria = "eDP-1";
    mode = "2880x1800@143.91Hz";
    scale = 1.5;
  };
  dellScreen = {
    criteria = "Dell Inc. DELL S2721DGF 4P11R83";
    mode = "2560x1440@120.00Hz";
    scale = 1.0;
  };

  enable = screen: screen // { status = "enable"; };
  disable = screen: screen // { status = "disable"; };
in
{
  home.packages = [ pkgs.kanshi ]; # need for kanshictl switch
  services.kanshi = {
    enable = true;
    systemdTarget = "hyprland-session.target";
    settings = [
       {
         profile.name = "undocked";
         profile.outputs = [
           (enable laptopScreen)
           (disable dellScreen)
         ];
       }
       {
         profile.name = "docked-office";
         profile.outputs = [
           (disable laptopScreen)
           (enable dellScreen)
         ];
       }
    ];
  };
}