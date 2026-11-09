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

  # Kanshi matches a profile only when its outputs pair off one-for-one with the connected heads,
  # and a head left out keeps whatever state it had, so there is a profile per set of screens
  # plugged in, each saying what all of them do. Add a screen to this list and to the profiles
  # below together, or the sets stop covering each other and nothing matches.
  screens = [ laptopScreen dellScreen livingRoomScreen ];

  # Two profiles pair with the same set of heads whenever a screen can be on or off with the rest
  # plugged in, and kanshi takes the first that matches: plugging a screen in should light it up,
  # so the profile turning it on is listed first. Selecting from the menu is by name and ignores
  # the order.
  profiles = [
    { name = "internal";             key = "1"; desc = "Laptop";              outputs = [ (enable laptopScreen) ]; }
    { name = "external-office";      key = "2"; desc = "Office monitor";      outputs = [ (disable laptopScreen) (enable dellScreen) ]; }
    { name = "internal-office";      key = "3"; desc = "Laptop, monitor off"; outputs = [ (enable laptopScreen) (disable dellScreen) ]; }
    { name = "external-living-room"; key = "4"; desc = "Living room TV";      outputs = [ (disable laptopScreen) (enable livingRoomScreen) ]; }
    { name = "internal-living-room"; key = "5"; desc = "Laptop, TV off";      outputs = [ (enable laptopScreen) (disable livingRoomScreen) ]; }
  ];

  # The screens a profile names, and those it expects unplugged: kanshi takes the first profile
  # whose outputs pair with the heads, so both hold only in the setup the profile is for.
  named = profile: map (output: output.criteria) profile.outputs;
  unplugged = profile: map (screen: screen.criteria)
    (lib.filter (screen: !lib.elem screen.criteria (named profile)) screens);
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
      # Any other set of screens and kanshi has no profile pairing the heads: it refuses the
      # switch, and the menu would look like nothing happened, so dim the entry instead.
      requires = {
        connected = named profile;
        disconnected = unplugged profile;
      };
    }) profiles ++ [
      { key = "c"; desc = "Configure..."; cmd = lib.getExe pkgs.wdisplays; }
    ]; }
  ];
}
