{ lib, pkgs, config, self, ... }:
let
  cfg = config.custom.desktop-environment.lock-screen;

  pidof = lib.getExe' pkgs.procps "pidof";
  niri = lib.getExe pkgs.niri;
  hyprlock = lib.getExe pkgs.hyprlock;
in
{
  options.custom.desktop-environment.lock-screen = {
    background = lib.mkOption {
      description = "Location of the wallpaper";
      type = lib.types.str;
    };

    font-family = lib.mkOption {
      description = "Font family";
      type = lib.types.str;
      default = config.stylix.fonts.sansSerif.name;
    };
  };

  config = {
    # TODO: check if niri enabled?
    custom.desktop-environment.session.lock = ''${pidof} hyprlock || ${niri} msg action do-screen-transition --delay-ms 750 && ${hyprlock}'';
    programs.hyprlock = {
      enable = true;
      settings = {
        general.disable_loading_bar = true;
        background = [{
          path = cfg.background;
          blur_passes = 3;
          blur_size = 12;
        }];
        input-field = [{
          size = "300, 50";
          valign = "bottom";
          position = "0%, 10%";

          outline_thickness = 1;
          placeholder_text = "Enter Password";

          font_color = "rgb(b6c4ff)";
          outer_color = "rgba(180, 180, 180, 0.5)";
          inner_color = "rgba(200, 200, 200, 0.1)";
          check_color = "rgba(247, 193, 19, 0.5)";
          fail_color = "rgba(255, 106, 134, 0.5)";

          fade_on_empty = false;

          shadow_color = "rgba(0, 0, 0, 0.1)";
          shadow_size = 7;
          shadow_passes = 2;
        }];
        label = [{
          text = ''cmd[update:1000] echo "<span font-weight='ultralight' >$(date +'%H:%M')</span>"'';

          font_size = 300;
          font_family = cfg.font-family;
          color = "rgb(b6c4ff)";
          position = "0%, 2%";
          valign = "center";
          halign = "center";
          shadow_color = "rgba(0, 0, 0, 0.1)";
          shadow_size = 20;
          shadow_passes = 2;
          shadow_boost = 0.3;
        }];
      };
    };
  };
}
