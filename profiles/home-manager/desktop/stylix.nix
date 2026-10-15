{ lib, pkgs, inputs, ... }:
{
  imports = [ inputs.stylix.homeModules.stylix ];

  stylix = {
    enable = true;
    autoEnable = false;

    base16Scheme = "${inputs.tinted-schemes}/base16/onedark.yaml";

    polarity = "dark";

    fonts = {
      monospace = {
        package = pkgs.nerd-fonts.hack;
        name = "Hack Nerd Font Mono";
      };

      sansSerif = {
        name = "Noto Sans";
        package = pkgs.noto-fonts;
      };

      serif = {
        name = "Noto Serif";
        package = pkgs.noto-fonts;
      };

      emoji = {
        package = pkgs.noto-fonts-color-emoji;
        name = "Noto Color Emoji";
      };

      sizes = {
        terminal = if pkgs.stdenv.hostPlatform.isDarwin then 10.5 else 15; # Ghostty scales by 4/3 on macOS
        applications = 12;
        desktop = 10;
        popups = 14;
      };
    };

    icons = {
      enable = pkgs.stdenv.hostPlatform.isLinux;
      package = pkgs.papirus-icon-theme;
      light = "Papirus-Light";
      dark = "Papirus-Dark";
    };

    cursor = {
      package = pkgs.bibata-cursors;
      name = "Bibata-Modern-Classic";
      size = 16;
    };

    opacity.terminal = lib.mkIf pkgs.stdenv.hostPlatform.isLinux 0.92; # Needed for niri blur to show through

    targets = {
      font-packages.enable = true;
    };
  };

  home.pointerCursor.enable = pkgs.stdenv.hostPlatform.isLinux;  # stylix only sets the cursor on Linux
}
