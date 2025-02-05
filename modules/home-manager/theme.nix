{ lib, pkgs, config, self, ... }:

let
  cfg = config.custom.theme;

  cursorModule = lib.types.submodule {
    options = {
      package = lib.mkOption { type = lib.types.package; };
      name = lib.mkOption { type = lib.types.str; };
      size = lib.mkOption { type = lib.types.int; };
    };
  };

  # https://github.com/nix-community/home-manager/blob/3b6fde96d8b41a4a21cbacba8de7debf7ea78021/modules/misc/qt.nix#L9
  qtModule = lib.types.submodule {
    options = {
      platform = lib.mkOption { type = lib.types.str; };
      style = lib.mkOption { type = lib.types.str; };
    };
  };

  fontModule = lib.types.submodule {
    options = {
      package = lib.mkOption { type = lib.types.package; };
      name = lib.mkOption { type = lib.types.str; };
    };
  };

  gtkThemeModule = lib.types.submodule {
    options = {
      package = lib.mkOption { type = lib.types.package; };
      name = lib.mkOption { type = lib.types.str; };
      prefer-dark-mode = lib.mkOption { type = lib.types.bool; };
    };
  };

  gtkIconModule = lib.types.submodule {
    options = {
      package = lib.mkOption { type = lib.types.package; };
      name = lib.mkOption { type = lib.types.str; };
    };
  };

  gtkModule = lib.types.submodule {
    options = {
      theme = lib.mkOption { type =gtkThemeModule; };
      icons = lib.mkOption { type = gtkIconModule; };
      font-size = lib.mkOption { type = lib.types.int; };
    };
  };

  fontsModule = lib.types.submodule {
    options = {
      monospace = lib.mkOption { type = fontModule; };
      sansSeriff = lib.mkOption { type = fontModule; };
      gui = lib.mkOption { type = fontModule; };
    };
  };

  terminalModule = lib.types.submodule {
    options = {
      foreground = lib.mkOption { type = lib.types.str; default = cfg.palette.base15; };
      background = lib.mkOption { type = lib.types.str; default = cfg.palette.base0; };
      selection-foreground = lib.mkOption { type = lib.types.str; default = cfg.palette.base15; };
      selection-background = lib.mkOption { type = lib.types.str; default = cfg.palette.base8; };

      cursor-color = lib.mkOption { type = lib.types.str; default = cfg.palette.base15; };
      cursor-text = lib.mkOption { type = lib.types.str; default = cfg.palette.base0; };

      font-size = lib.mkOption { type = lib.types.int; default = 15; };
    };
  };

  paletteModule = lib.types.submodule {
    options = {
      error = lib.mkOption { type = lib.types.str; };

      # Black
      base0 = lib.mkOption { type = lib.types.str; };
      base8 = lib.mkOption { type = lib.types.str; };

      # Red
      base1 = lib.mkOption { type = lib.types.str; };
      base9 = lib.mkOption { type = lib.types.str; };

      # Green
      base2 = lib.mkOption { type = lib.types.str; };
      base10 = lib.mkOption { type = lib.types.str; };

      # Yellow
      base3 = lib.mkOption { type = lib.types.str; };
      base11 = lib.mkOption { type = lib.types.str; };

      # Blue
      base4 = lib.mkOption { type = lib.types.str; };
      base12 = lib.mkOption { type = lib.types.str; };

      # Magenta
      base5 = lib.mkOption { type = lib.types.str; };
      base13 = lib.mkOption { type = lib.types.str; };

      # Cyan
      base6 = lib.mkOption { type = lib.types.str; };
      base14 = lib.mkOption { type = lib.types.str; };

      # White
      base7 = lib.mkOption { type = lib.types.str; };
      base15 = lib.mkOption { type = lib.types.str; };
    };
  };
in
{
  options.custom.theme = {
    terminal = lib.mkOption { type = terminalModule; };
    palette = lib.mkOption { type = paletteModule; };
    fonts = lib.mkOption { type = fontsModule; };
    cursor = lib.mkOption { type = cursorModule; };
    gtk = lib.mkOption { type = gtkModule; };
    qt = lib.mkOption { type = qtModule; };
  };

  config = {
    home.packages = [
      cfg.fonts.monospace.package
      cfg.fonts.sansSeriff.package
      cfg.fonts.gui.package
    ];

    fonts.fontconfig = lib.mkIf config.fonts.fontconfig.enable {
      defaultFonts = {
        monospace = [ cfg.fonts.monospace.name ];
        # serif = [ ... ];
        sansSerif = [ cfg.fonts.sansSeriff.name ];
      };
    };

    home.pointerCursor = lib.mkIf pkgs.stdenv.isLinux (cfg.cursor // { gtk.enable = config.gtk.enable; });

    gtk = lib.mkIf config.gtk.enable {
      theme = { inherit (cfg.gtk.theme) package name; };
      iconTheme = { inherit (cfg.gtk.icons) package name; };
      font = {
        inherit (cfg.fonts.gui) package name;
        size = cfg.gtk.font-size;
      };

      gtk3.extraConfig.gtk-application-prefer-dark-theme = if (cfg.gtk.theme.prefer-dark-mode) then 1 else 0;
      gtk4.extraConfig.gtk-application-prefer-dark-theme = if (cfg.gtk.theme.prefer-dark-mode) then 1 else 0;
    };

    qt = lib.mkIf config.qt.enable {
      platformTheme.name = cfg.qt.platform;
      style.name = cfg.qt.style;
    };
  };
}