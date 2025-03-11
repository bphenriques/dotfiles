{ pkgs, lib, self, config, ... }:
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
  livingRoomScreen = {
    criteria = "LG Electronics LG TV SSCR2 0x01010101";
    mode = "2560x1440@119.998";
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
         profile = {
           name = "internal";
           outputs = [ (enable laptopScreen) ];
         };
       }
       {
         profile = {
           name = "external-office";
           outputs = [ (disable laptopScreen) (enable dellScreen) ];
         };
       }
       {
         profile = {
           name = "external-living-room";
           outputs = [ (disable laptopScreen) (enable livingRoomScreen) ];
         };
       }
   ];
  };

  custom.programs.niri.output.default = {
    identifier  = "eDP-1";
    resolution  = "2880x1800";
    refreshRate = "120.001"; # using float leads to trailing zeros that I dont want
    scale       = "1.75";
  };
}
