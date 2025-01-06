{ pkgs, lib, ... }:

# desktop items: https://github.com/edmundmiller/dotfiles/blob/main/modules/desktop/apps/rofi.nix
let
  theme = ''
  * {
    font: "Cascadia Code NF 11";

    bg0: #151515f2;
    bg1: #1f1f1f80;
    bg2: #393939bf;
    fg0: #ffffff;
    fg1: #cecece;
    fg2: #DEDEDE80;

    background-color: transparent;
    text-color: @fg0;

    margin: 0;
    padding: 0;
    spacing: 0;
  }

  window {
    background-color: @bg0;
    location: center;
    width: 540px;
    border-radius: 4px;
  }

  inputbar {
    font: "Cascadia Code NF 13";
    padding: 12px;
    spacing: 12px;
    children: [ icon-search, entry];
  }

  icon-search {
    expand: false;
    filename: "search";
    size: 28px;
  }

  icon-search,
  entry,
  element-icon,
  element-text {
    vertical-align: 0.5;
  }

  entry {
    font: inherit;

    placeholder: "Search";
    placeholder-color: @fg2;
  }

  message {
    border: 2px 0 0;
    border-color: @bg1;
    background-color: @bg1;
  }

  textbox {
    padding: 8px 24px;
  }

  listview {
    lines: 7;
    columns: 1;
    fixed-height: false;
    border: 1px 0 0;
    border-color: @bg1;
  }

  element {
    padding: 8px 16px;
    spacing: 16px;
    background-color: transparent;
    children: [ element-icon, element-text];
  }

  element normal active {
    text-color: @bg2;
  }

  element alternate active {
    text-color: @bg2;
  }

  element selected normal,
  element selected active {
    background-color: @bg2;
    text-color: @fg1;
  }

  element-icon {
    size: 2em;
  }

  element-text {
    text-color: inherit;
  }
  '';
in
{
  #   #https://codeberg.org/adamcstephens/dotfiles/src/branch/main/apps/rofi/default.nix
  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;
#    theme = {
#      "*" = {
#        font = "FiraCode Nerd Font Medium 14";
#        bg0 = mkLiteral "#1e1e2e";
#        bg1 = mkLiteral "#181825";
#        bg2 = mkLiteral "#11111b";
#        bg3 = mkLiteral "#b4befe";
#        fg0 = mkLiteral "#cdd6f4";
#        fg1 = mkLiteral "#bac2de";
#        fg2 = mkLiteral "#a6adc8";
#        red = mkLiteral "#f38ba8";
#        green = mkLiteral "#a6e3a1";
#        yellow = mkLiteral "#f9e2af";
#        blue = mkLiteral "#89b4fa";
#        magenta = mkLiteral "#cba6f7";
#        cyan = mkLiteral "#94e2d5";
#
#        accent = mkLiteral "@red";
#        urgent = mkLiteral "@yellow";
#
#        background-color = mkLiteral "transparent";
#        text-color = mkLiteral "@fg0";
#
#        margin = 0;
#        padding = 0;
#        spacing = 0;
#      };
#      "element-icon, element-text, scrollbar" = {
#        cursor = mkLiteral "pointer";
#      };
#
#      "window" = {
#        location = mkLiteral "center";
#        width = mkLiteral "500px";
#
#        background-color = mkLiteral "@bg1";
#        border = mkLiteral "3px";
#        border-color = mkLiteral "@bg3";
#        border-radius = mkLiteral "6px";
#      };
#
#      "inputbar" = {
#        spacing = mkLiteral "8px";
#        padding = mkLiteral "4px 8px";
#        children = mkLiteral "[ prompt, entry ]";
#
#        background-color = mkLiteral "@bg0";
#      };
#
#      "icon-search, entry, element-icon, element-text" = {
#        vertical-align = mkLiteral "0.5";
#      };
#
#      "icon-search" = {
#        expand = mkLiteral "false";
#        filename = "search-symbolic";
#        size = mkLiteral "14px";
#      };
#
#      "textbox" = {
#        padding = mkLiteral "4px 8px";
#        background-color = mkLiteral "@bg2";
#      };
#
#      "listview" = {
#        padding = mkLiteral "4px 0px";
#        lines = mkLiteral "12";
#        columns = mkLiteral "1";
#        scrollbar = mkLiteral "false";
#        fixed-height = mkLiteral "false";
#        dynamic = mkLiteral "true";
#      };
#
#      "element" = {
#        padding = mkLiteral "4px 8px";
#        spacing = mkLiteral "8px";
#      };
#
#      "element normal urgent" = {
#        text-color = mkLiteral "@urgent";
#      };
#
#      "element normal active" = {
#        text-color = mkLiteral "@accent";
#      };
#
#      "element selected" = {
#        text-color = mkLiteral "@bg1";
#        background-color = mkLiteral "@accent";
#      };
#
#      "element selected urgent" = {
#        background-color = mkLiteral "@urgent";
#      };
#
#      "element-icon" = {
#        size = mkLiteral "0.8em";
#      };
#
#      "element-text" = {
#        text-color = mkLiteral "inherit";
#      };
#
#      "scrollbar" = {
#        handle-width = mkLiteral "0px";
#        handle-color = mkLiteral "@fg2";
#        padding = mkLiteral "0 4px";
#      };
#    };
#    extraConfig = {
#      modes = mkLiteral "[ combi ]";
#      combi-modes = mkLiteral "[ window, drun, run ]";
#    };
  };
}

# https://github.com/iynaix/dotfiles/blob/e441ab4ff7a775b57b6c79a2fa6be99e3ab2d58b/home-manager/programs/rofi.nix
# https://github.com/iynaix/dotfiles/blob/e441ab4ff7a775b57b6c79a2fa6be99e3ab2d58b/home-manager/programs/rofi-wifi-menu.sh
# https://github.com/iynaix/dotfiles/blob/e441ab4ff7a775b57b6c79a2fa6be99e3ab2d58b/home-manager/programs/rofi-power-menu.sh
# https://github.com/adi1090x/rofi/tree/master/files
# https://github.com/edmundmiller/dotfiles/blob/main/modules/desktop/apps/rofi.nix
# https://github.com/edmundmiller/dotfiles/blob/main/config/rofi/bin/rofi-browsermenu
