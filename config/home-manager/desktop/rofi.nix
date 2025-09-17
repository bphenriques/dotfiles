{ config, pkgs, self, lib, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  stylix.targets.rofi.enable = true;
  programs.rofi = {
    enable = true;
    theme = let
      inherit (config.lib.formats.rasi) mkLiteral;
    in {
      # Overwrite stylix elements to remove alternate coloring.
      "element alternate.normal" = {
        background-color = lib.mkForce (mkLiteral "@normal-background");
        text-color = lib.mkForce (mkLiteral "@normal-text");
      };

      "element alternate.active" = {
        background-color = lib.mkForce (mkLiteral "@active-background");
        text-color = lib.mkForce (mkLiteral "@active-text");
      };
    };
  };

  xdg.configFile = {
    "rofi/wallpaper.rasi".text = ''
      @theme "custom"

      window {
        transparency:   "real";
        location:       center;
        anchor:         center;
        fullscreen:     false;
        border-radius:  10px;
      }

      mainbox {
        enabled:        true;
        spacing:        10px;
        padding:        30px;
        children:       [ "inputbar", "listview-split" ];
      }

      inputbar {
        enabled:        true;
        spacing:        10px;
        children:       [ "textbox-prompt-colon", "entry" ];
      }

      textbox-prompt-colon {
        enabled:  true;
        padding:  5px 0px;
        expand:   false;
        str:      "";
      }

      entry {
        enabled:  true;
        padding:  5px 0px;
      }

      scrollbar {
        handle-width:     5px ;
        border-radius:    10px;
        background-color: @alternate-normal-background;
      }

      listview-split {
        background-color: transparent;
        cycle:            true;
        dynamic:          true;
        orientation:      horizontal;
        children:         [ listview, icon-current-entry ];
      }

      listview {
        enabled:          true;
        dynamic:          true;
        scrollbar:        true;
        layout:           vertical;
        reverse:          false;
        spacing:          5px;
      }

      icon-current-entry {
        enabled:  true;
        size:     40%;
        padding:  10px;
      }

      configuration {
        modi: "filebrowser";
        filebrowser {
          directories-first: false;
          directory: "${self.pkgs.wallpapers}/share/wallpapers";
          command: "${lib.getExe self.pkgs.swww-util} one";
        }
      }
    '';
  };

  home.packages = [
    (pkgs.makeDesktopItem {
      name = "browse-wallpapers";
      desktopName = "Browse Wallpapers";
      icon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; } "wallpaper" "󰸉";
      exec = ''${lib.getExe config.programs.rofi.package} -show filebrowser -theme ${config.xdg.configHome}/rofi/wallpaper.rasi'';
    })
  ];
}
