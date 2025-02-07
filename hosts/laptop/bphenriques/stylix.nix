{ pkgs, config, self, ... }:
let
  wallpapersPkg = self.pkgs.wallpapers.override {
    selected = [ "sunrise-mountains" "lake-fishing-sunset" ];
  };
in rec {
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

    targets.qt.platform = "gnome"; # It uses adwait or adwait-dark underneath
  };
}

#--main-color: #38474f;
#  --accent-color: #ffe57f;
#  --text-color: #FAFAFA;