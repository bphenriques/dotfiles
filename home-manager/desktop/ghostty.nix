{ config, pkgs, lib, self, ... }:

# TODO: https://github.com/nix-community/home-manager/commit/5f6aa268e419d053c3d5025da740e390b12ac936
let
  theme = self.themes.doom-one;
  font = theme.font.monospace;
in
{
  home.packages = lib.optionals pkgs.stdenv.isLinux [
    pkgs.ghostty
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

    background = ${theme.background}
    foreground = ${theme.foreground}
    selection-foreground = ${theme.selectionForeground}
    selection-background = ${theme.selectionBackground}
    cursor-color = ${theme.cursor}
    cursor-text = ${theme.cursorText}

    # black
    palette = 0=${theme.color0}
    palette = 8=${theme.color8}
    # red
    palette = 1=${theme.color1}
    palette = 9=${theme.color9}
    # green
    palette = 2=${theme.color2}
    palette = 10=${theme.color10}
    # yellow
    palette = 3=${theme.color3}
    palette = 11=${theme.color11}
    # blue
    palette = 4=${theme.color4}
    palette = 12=${theme.color12}
    # purple
    palette = 5=${theme.color5}
    palette = 13=${theme.color13}
    # aqua
    palette = 6=${theme.color6}
    palette = 14=${theme.color14}
    # white
    palette = 7=${theme.color7}
    palette = 15=${theme.color15}

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


