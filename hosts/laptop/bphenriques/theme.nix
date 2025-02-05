{ pkgs }:
rec {
  terminal = {
    foreground            = palette.base15;
    background            = palette.base0;
    selection-foreground  = palette.base15;
    selection-background  = palette.base8;
    cursor-color          = palette.base15;
    cursor-text           = palette.base0;

    font-size             = 15;
  };

  palette = {
    error = "#b66467";

    # Black
    base0 = "#282c34";
    base8 = "#3f444a";

    # Red
    base1 = "#ff6c6b";
    base9 = "#ff6655";

    # Green
    base2  = "#98be65";
    base10 = "#99bb66";

    # Yellow
    base3  = "#ECBE7B";
    base11 = "#ECBE7B";

    # Blue
    base4  = "#51afef";
    base12 = "#51afef";

    # Magenta
    base5  = "#c678dd";
    base13 = "#c678dd";

    # Cyan
    base6  = "#46D9FF";
    base14 = "#46D9FF";

    # White
    base7  = "#dfdfdf";
    base15 = "#bbc2cf";
  };

  fonts = {
    monospace = {
      package = pkgs.nerd-fonts.hack;
      name = "Hack Nerd Font Mono";
    };

    sansSeriff = {
      package = pkgs.source-sans-pro;
      name = "Source Sans Pro";
    };

    gui = {
      package = pkgs.nerd-fonts.ubuntu;
      name = "Ubuntu Nerd Font";
    };
  };

  cursor = {
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Classic";
    size = 16;
  };

  gtk = {
    theme = {
      package = pkgs.adw-gtk3;
      name = "adw-gtk3-dark";
      prefer-dark-mode = true;
    };

    icons = {
      package = pkgs.morewaita-icon-theme;
      name = "MoreWaita";
    };

    font-size = 11;
  };

  qt = {
    platform = "adwaita";
    style = "adwaita-dark";
  };
}