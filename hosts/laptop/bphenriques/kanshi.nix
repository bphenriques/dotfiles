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

  enable = screen: screen // { status = "enable"; };
  disable = screen: screen // { status = "disable"; };

  mkIcon = name: symbol: self.lib.builders.mkNerdFontIcon pkgs { textColor = config.lib.stylix.colors.withHashtag.base07; } name symbol;

  notify = { msg, icon }: ''${lib.getExe pkgs.libnotify} --expire-time 5000 --icon "${icon}" --category "kanshi-osd" --hint string:x-canonical-private-synchronous:kanshi-osd --hint string:x-dunst-stack-tag:kanshi-osd "${msg}"'';
in
{
  services.kanshi = {
    enable = true;
    settings = [
       {
         profile = {
           name = "internal";
           outputs = [ (enable laptopScreen) ];
           exec = notify { msg = "Laptop Screen"; icon = mkIcon "kanshi-internal" ""; };
         };
       }
       {
         profile = {
           name = "external-office";
           outputs = [ (disable laptopScreen) (enable dellScreen) ];
           exec = notify { msg = "Office Display"; icon = mkIcon "kanshi-external-office" "󰍹"; };
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