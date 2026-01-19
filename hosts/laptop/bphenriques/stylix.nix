{ pkgs, ... }:
{
  stylix = {
    enable = true;
    autoEnable = false;

    base16Scheme = "${pkgs.base16-schemes}/share/themes/onedark.yaml";
    override.base00 = "#282c34"; # Background

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
        terminal = 15;
        applications = 12;
        desktop = 10;
        popups = 14;
      };
    };

    icons = {
      enable = true;
      package = pkgs.papirus-icon-theme;
      light = "Papirus-Light";
      dark = "Papirus-Dark";
    };

    cursor = {
      package = pkgs.bibata-cursors;
      name = "Bibata-Modern-Classic";
      size = 16;
    };

    opacity = {
      applications = 1.0;
      popups = 1.0;
      terminal = 1.0;
      desktop = 1.0;
    };

    targets = {
      font-packages.enable = true;
      qt.platform = "gnome"; # Stylix does not support adwaita.
    };
  };
}
