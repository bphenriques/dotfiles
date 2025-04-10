{ pkgs, config, self, ... }:
let
  background = "${self.pkgs.wallpapers}/share/wallpapers/lake.jpg";
  lockPackage = pkgs.writeShellApplication {
    name = "session-lock";
    runtimeInputs = [
      pkgs.niri
      pkgs.procps
      pkgs.hyprlock
    ];
    text = ''pidof hyprlock || niri msg action do-screen-transition --delay-ms 750 && hyprlock'';
  };
in
{
  custom.programs.session.exec.lock = lockPackage;
  programs.hyprlock = {
    enable = true;
    settings = {
      general.disable_loading_bar = true;
      background = [{
        path = background;
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

        font_size = 350;
        font_family = config.stylix.fonts.sansSerif.name;
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
}
