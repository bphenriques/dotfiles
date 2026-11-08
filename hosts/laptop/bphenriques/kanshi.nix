{ lib, pkgs, ... }:
let
  kanshictl = lib.getExe' pkgs.kanshi "kanshictl";
  brightnessctl = "${lib.getExe pkgs.brightnessctl} --class=backlight";
  brightness = [ "${brightnessctl} set 5%-" "${brightnessctl} set 5%+" ];

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

  profiles = [
    { name = "internal";             key = "1"; desc = "Laptop";         outputs = [ (enable laptopScreen) ]; }
    { name = "external-office";      key = "2"; desc = "Office monitor"; outputs = [ (disable laptopScreen) (enable dellScreen) ]; }
    { name = "external-living-room"; key = "3"; desc = "Living room TV"; outputs = [ (disable laptopScreen) (enable livingRoomScreen) ]; }
  ];
in
{
  services.kanshi = {
    enable = true;
    settings = map (profile: { profile = { inherit (profile) name outputs; }; }) profiles;
  };

  my.programs.desktop-shell.settings = {
    osd.adjust = { inherit brightness; };
    commands.displayProfile = "${kanshictl} status | ${lib.getExe pkgs.gnused} -n 's/^Current profile: //p'";
  };

  my.programs.desktop-shell.menu.entries = lib.mkOrder 500 [
    { key = "d"; desc = "Display"; status = "display"; submenu = [
      # The backlight only dims the laptop screen, so it does nothing while a profile has it off.
      { key = [ "Down" "Up" ]; desc = "Brightness"; cmd = brightness; keepOpen = true; requires.enabled = [ laptopScreen.criteria ]; }
    ] ++ map (profile: {
      inherit (profile) key desc;
      profile = profile.name;
      cmd = "${kanshictl} switch ${profile.name}";
      # A profile can only apply while the screens it turns on are connected.
      requires.connected = map (output: output.criteria) (lib.filter (output: output.status == "enable") profile.outputs);
    }) profiles ++ [
      { key = "c"; desc = "Configure..."; cmd = lib.getExe pkgs.wdisplays; }
    ]; }
  ];
}
