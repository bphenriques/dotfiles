{ lib, pkgs, config, self, osConfig, ... }:

let
  inherit (builtins) listToAttrs replaceStrings;
  inherit (lib) map;
  inherit (lib.attrsets) nameValuePair;
  inherit (config.custom.desktop-environment) settings;

  cfg = config.custom.desktop-environment.apps;

  volume = lib.getExe self.pkgs.volume-osd;
  brightness = lib.getExe self.pkgs.brightness-osd;
  playerctl = lib.getExe pkgs.playerctl;
  dmenu = "${lib.getExe pkgs.fuzzel} -d";
  terminal = lib.getExe' config.programs.foot.package "footclient";
  date = lib.getExe' pkgs.coreutils "date";

  mkAppOpt = { description ? "", default ? null }: lib.mkOption {
    inherit description default;
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  mkAppOpt' = default: mkAppOpt { inherit default; description = ""; };
in
{
  options.custom.desktop-environment.apps = {
    volume = {
      increase    = mkAppOpt' "${volume} increase";
      decrease    = mkAppOpt' "${volume} decrease";
      toggle-mute = mkAppOpt' "${volume} toggle-mute";
    };

    brightness = {
      increase = mkAppOpt' "${brightness} increase";
      decrease = mkAppOpt' "${brightness} decrease";
    };

    mediaPlayer = {
      previous      = mkAppOpt' "${playerctl} previous";
      next          = mkAppOpt' "${playerctl} next";
      play-pause    = mkAppOpt' "${playerctl} toggle-pause";
    };

    core = {
      application-launcher  = mkAppOpt' pkgs.fuzzel;
      dmenu                 = mkAppOpt' dmenu;
      file-browser          = mkAppOpt { description = "File Browser"; };
      terminal              = mkAppOpt { description = "Terminal"; };
    };

    tools = {
      system-monitor        = mkAppOpt { };
      emoji-picker          = mkAppOpt' (pkgs.writeShellApplication {
        name = "emoji-picker";
        runtimeInputs = [ pkgs.wtype ];
        text = ''BEMOJI_PICKER_CMD="${dmenu}" ${lib.getExe pkgs.bemoji} --noline --type --clip'';
      });
    };

    wm = {
      focused-output = mkAppOpt { };
    };
  };

  config = {
    home.packages = [
      (pkgs.makeDesktopItem {
        name = "system-monitor";
        desktopName = "System Monitor";
        icon = "folder";  # FIXME
        exec =  cfg.tools.system-monitor;
      })

      (pkgs.makeDesktopItem {
        name = "File Browser";
        desktopName = "Open file browser";
        icon = "folder";  # FIXME
        exec = cfg.core.file-browser;
        actions = let
          bookmarkToAction = b: nameValuePair (replaceStrings [" "] ["-"] b.name) {
            inherit (b) name;
            exec = "${cfg.core.file-browser} ${b.path}";
          };
        in listToAttrs (lib.map bookmarkToAction settings.file-bookmarks);
      })
    ];
  };
}
