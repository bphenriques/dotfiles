_:
let
  mkScreen = { criteria, resolution, refreshRate, scale }: {
    inherit criteria resolution refreshRate scale;
    mode = "${resolution}@${refreshRate}Hz";
  };

  laptopScreen = mkScreen {
    criteria = "eDP-1";
    resolution = "2880x1800";
    refreshRate = "120.001";
    scale = 1.75;
  };
  dellScreen = mkScreen {
    criteria = "Dell Inc. DELL S2721DGF 4P11R83";
    resolution = "2560x1440";
    refreshRate = "143.92";
    scale = 1.0;
  };
  livingRoomScreen = mkScreen {
    criteria = "LG Electronics LG TV SSCR2 0x01010101";
    resolution = "2560x1440";
    refreshRate = "119.998";
    scale = 1.0;
  };

  toKanshi = screen: { inherit (screen) criteria mode scale; };
  enable = screen: toKanshi screen // { status = "enable"; };
  disable = screen: toKanshi screen // { status = "disable"; };
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
    inherit (laptopScreen) resolution;
    inherit (laptopScreen) refreshRate;
    scale       = toString laptopScreen.scale;
  };
}
