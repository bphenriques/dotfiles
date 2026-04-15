_:
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
    mode = "2560x1440@119.998Hz";
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
    identifier  = laptopScreen.criteria;
    resolution  = builtins.head (builtins.split "@" laptopScreen.mode);
    refreshRate = builtins.replaceStrings [ "Hz" ] [ "" ] (builtins.elemAt (builtins.split "@" laptopScreen.mode) 2);
    scale       = toString laptopScreen.scale;
  };
}
