{ pkgs, config, lib, ... }:
let
  colors = config.lib.stylix.colors;
  font = config.stylix.fonts.monospace.name;
  # Without an outline hyprlock colours the whole box on failure and Caps Lock, so only a faint tint.
  tint = base: "rgba(${colors."${base}-rgb-r"}, ${colors."${base}-rgb-g"}, ${colors."${base}-rgb-b"}, 0.2)";
  lockPackage = pkgs.writeShellApplication {
    name = "session-lock";
    runtimeInputs = [
      pkgs.niri
      pkgs.procps
      pkgs.hyprlock
    ];
    text = ''pidof hyprlock || { niri msg action do-screen-transition --delay-ms 750 && hyprlock; }'';
  };
  # hyprlock's $LAYOUT stays empty under niri, so ask niri for the current layout.
  keyboardLayout = pkgs.writeShellApplication {
    name = "lock-keyboard-layout";
    runtimeInputs = [ pkgs.niri pkgs.jq ];
    text = ''
      niri msg --json keyboard-layouts | jq -r --argjson names '${builtins.toJSON (map (l: l.label) config.my.programs.niri.keyboardLayouts)}' \
        '$names[.current_idx] // ""'
    '';
  };
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  my.programs.session.exec.lock = lib.getExe lockPackage;
  # Black, a username and a password prompt: the same look as the tuigreet login.
  programs.hyprlock = {
    enable = true;
    settings = {
      general = {
        hide_cursor = true;
      };
      background = [{ color = "rgb(000000)"; }];
      label = [
        {
          text = "$USER";
          font_family = font;
          font_size = 28;
          color = "rgb(${colors.base05})";
          position = "0, 60";
          halign = "center";
          valign = "center";
        }
        {
          text = "cmd[update:1000] ${lib.getExe keyboardLayout}";
          font_family = font;
          font_size = 16;
          color = "rgb(${colors.base03})";
          # Inside the password box, at its right edge.
          position = "170, -20";
          zindex = 1;
          halign = "center";
          valign = "center";
        }
      ];
      input-field = [{
        size = "400, 60";
        position = "0, -20";
        halign = "center";
        valign = "center";
        outline_thickness = 0;
        dots_center = true;
        fade_on_empty = false;
        placeholder_text = "_";
        fail_text = ''<span foreground="##${colors.base08}">$FAIL ($ATTEMPTS)</span>'';
        font_family = font;
        font_color = "rgb(${colors.base05})";
        inner_color = "rgb(000000)";
        check_color = "rgb(000000)";
        fail_color = tint "base08";
        capslock_color = tint "base0A";
      }];
    };
  };
}
