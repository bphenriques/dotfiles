{ lib, pkgs, config, self, osConfig, ... }:
let
  inherit (builtins) listToAttrs replaceStrings;
  inherit (lib) map nameValuePair;

  cfg = config.custom.programs.screen-recorder;

  mkAppOpt = default: lib.mkOption {
    inherit default;
    description = "";
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  cmd = desc: cmd: { inherit desc cmd; };
  submenu = desc: submenu: { inherit desc submenu; };
  mkIcon = name: symbol: self.lib.builders.mkNerdFontIcon pkgs { textColor = config.lib.stylix.colors.withHashtag.base07; } name symbol;

  screen-recorder = lib.getExe cfg.package;
  screenRecordingActions = [
    { id = "recording-screen-audio";    symbol = "󰹑"; label = "Screen (with audio)";  exec = cfg.exec.screen-audio; }
    { id = "recording-screen-no-audio"; symbol = "󰹑"; label = "Screen (no audio)";    exec = cfg.exec.screen-no-audio; }
    { id = "recording-region-audio";    symbol = ""; label = "Region (with audio)";  exec = cfg.exec.region-audio; }
    { id = "recording-region-no-audio"; symbol = ""; label = "Region (no audio)";    exec = cfg.exec.region-no-audio; }
    { id = "recording-stop";            symbol = ""; label = "Stop";                 exec = cfg.exec.stop; }
  ];

  dmenu = self.lib.builders.writeDmenuApplication pkgs {
    name = "screen-recorder-menu";
    entries = lib.map (e: { inherit (e) exec; label = "${e.symbol}     ${e.label}"; }) screenRecordingActions;
  };
in
{
  options.custom.programs.screen-recorder = {
    enable = lib.mkEnableOption "custom-screen-recorder";
    package = lib.mkOption {
      type = lib.types.package;
      default = self.pkgs.screen-recorder.override {
        recordIcon = mkIcon "screen-recorder-record-icon" "󰑋";
        informationIcon = mkIcon "screen-recorder-information-icon" "";
        errorIcon = mkIcon "screen-recorder-error-icon" "";
      };
    };
    directory = lib.mkOption {
      description = "Location of recordings";
      type = lib.types.str;
      default = config.xdg.userDirs.extraConfig.XDG_RECORDINGS_DIR;
    };

    exec = {
      menu             = mkAppOpt ''${lib.getExe dmenu}'';
      screen-audio     = mkAppOpt ''${screen-recorder} screen-audio "${cfg.directory}"'';
      screen-no-audio  = mkAppOpt ''${screen-recorder} screen-no-audio "${cfg.directory}"'';
      region-audio     = mkAppOpt ''${screen-recorder} region-audio "${cfg.directory}"'';
      region-no-audio  = mkAppOpt ''${screen-recorder} region-no-audio "${cfg.directory}"'';
      stop             = mkAppOpt ''${screen-recorder} stop'';
    };
  };

  config = lib.mkIf cfg.enable {
   assertions = [ (lib.hm.assertions.assertPlatform "custom.programs.screen-recorder" pkgs lib.platforms.linux) ];

    home.packages = [
      pkgs.wl-screenrec
      cfg.package
      dmenu
      (pkgs.makeDesktopItem {
        name = "screen-recoder-menu";
        desktopName = "Screen Recording";
        icon = mkIcon "screen-recorder" "󰑋";
        exec = lib.getExe dmenu;
        actions = let
          toAction = b: nameValuePair b.id {
            name = b.label;
            icon = mkIcon b.id b.symbol;
            exec = b.exec;
          };
        in listToAttrs (lib.map toAction screenRecordingActions);
      })
    ];

    custom.programs.wlr-which-key.menus.screen-recorder = {
      "Ctrl+s" = cmd "Save current recording" "${screen-recorder} stop";
      s = submenu "Screen" {
        a = cmd "with audio"  ''${screen-recorder} screen-audio "${cfg.directory}"'';
        m = cmd "no audio"    ''${screen-recorder} screen-no-audio "${cfg.directory}"'';
      };
      r = submenu "Region" {
        a = cmd "with audio"  ''${screen-recorder} region-audio "${cfg.directory}"'';
        m = cmd "no audio"    ''${screen-recorder} region-no-audio "${cfg.directory}"'';
      };
    };
  };
}
