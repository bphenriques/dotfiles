{ pkgs, config, self, ... }:
rec {
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
        package = pkgs.noto-fonts-emoji;
        name = "Noto Color Emoji";
      };

      sizes = {
        terminal = 15;
        applications = 12;
        desktop = 10;
        popups = 10;
      };
    };

    #      serif.package = pkgs.dejavu_fonts;
         #      serif.name = "DejaVu Serif";
         #
         #      sansSerif.package = pkgs.fira;
         #      sansSerif.name = "Fira Sans";
         #
         #      monospace.package = pkgs.jetbrains-mono;
         #      monospace.name = "JetBrains Mono";

    cursor = {
      package = pkgs.catppuccin-cursors; #pkgs.bibata-cursors;
      name = "frappeDark";#"Bibata-Modern-Classic";
      size = 16;
    };

    opacity = {
      applications = 1.0;
      popups = 1.0;
      terminal = 1.0;
      desktop = 1.0;
    };

    targets.qt.platform = "gnome"; # It uses adwait or adwait-dark underneath
  };
}

