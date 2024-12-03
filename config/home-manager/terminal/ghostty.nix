{ config, pkgs, lib, community, ... }:
let 
  font = {
    name = "Hack Nerd Font Mono";
    size = 15;
  };

  colors = {
    foreground            = "#bbc2cf";
    background            = "#282c34";
    selectionForeground   = "#bbc2cf";
    selectionBackground   = "#3f444a";

    cursor           = "#bbc2cf";
    cursorText       = "#282c34";

    # Black
    color0 = "#282c34";
    color8 = "#3f444a";

    # Red
    color1 = "#ff6c6b";
    color9 = "#ff6655";#

    # Green
    color2  = "#98be65";
    color10 = "#99bb66";

    # Yellow
    color3  = "#ECBE7B";
    color11 = "#ECBE7B";

    # Blue
    color4  = "#51afef";
    color12 = "#51afef";

    # Magenta
    color5  = "#c678dd";
    color13 = "#c678dd";

    # Cyan
    color6  = "#46D9FF";
    color14 = "#46D9FF";

    # White
    color7  = "#dfdfdf";
    color15 = "#bbc2cf";
  };
in
{
  # MacOS requires installation by hand for now: https://github.com/ghostty-org/ghostty/releases/tag/tip
  home.packages = lib.optionals pkgs.stdenv.isLinux [
    community.pkgs.ghostty
  ];

  xdg.mimeApps.defaultApplications = {
    "x-scheme-handler/terminal" = [ "Ghostty.desktop" ];
    "x-scheme-handler/x-executable" = [ "Ghostty.desktop" ];
  };

  programs.fish.interactiveShellInit = lib.optionalString pkgs.stdenv.isDarwin ''
    fish_add_path --append --move ${config.home.homeDirectory}/Applications/Ghostty.app/Contents/MacOS
  '';

  xdg.configFile."ghostty/config".text = ''
    font-family = "${font.name}"
    font-size = ${toString font.size}

    background = ${colors.background}
    foreground = ${colors.foreground}
    selection-foreground = ${colors.selectionForeground}
    selection-background = ${colors.selectionBackground}
    cursor-color = ${colors.cursor}
    cursor-text = ${colors.cursorText}

    # black
    palette = 0=${colors.color0}
    palette = 8=${colors.color8}
    # red
    palette = 1=${colors.color1}
    palette = 9=${colors.color9}
    # green
    palette = 2=${colors.color2}
    palette = 10=${colors.color10}
    # yellow
    palette = 3=${colors.color3}
    palette = 11=${colors.color11}
    # blue
    palette = 4=${colors.color4}
    palette = 12=${colors.color12}
    # purple
    palette = 5=${colors.color5}
    palette = 13=${colors.color13}
    # aqua
    palette = 6=${colors.color6}
    palette = 14=${colors.color14}
    # white
    palette = 7=${colors.color7}
    palette = 15=${colors.color15}

    copy-on-select = clipboard
  ''+ lib.optionalString pkgs.stdenv.isLinux ''
    gtk-single-instance = true
    window-decoration = true
  ''
  + lib.optionalString pkgs.stdenv.isDarwin ''
    window-colorspace = "display-p3"
    macos-non-native-fullscreen = visible-menu
    macos-option-as-alt = left
    mouse-hide-while-typing = true
  '';
}


