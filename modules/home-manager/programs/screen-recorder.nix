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

  screen-recorder = lib.getExe self.pkgs.screen-recorder;
  screenRecordingActions = [
    { id = "recording-screen-audio";    symbol = "󰹑"; label = "Screen (with audio)";  exec = cfg.screen-audio; }
    { id = "recording-screen-no-audio"; symbol = "󰹑"; label = "Screen (no audio)";    exec = cfg.screen-no-audio; }
    { id = "recording-region-audio";    symbol = ""; label = "Region (with audio)";  exec = cfg.region-audio; }
    { id = "recording-region-no-audio"; symbol = ""; label = "Region (no audio)";    exec = cfg.region-no-audio; }
    { id = "recording-stop";            symbol = ""; label = "Stop";                 exec = cfg.stop; }
  ];
in
{
  options.custom.programs.screen-recorder = {
    enable = lib.mkEnableOption "customn-screen-recorder";
    directory = lib.mkOption {
      description = "Location of recordings";
      type = lib.types.str;
      default = config.xdg.userDirs.extraConfig.XDG_RECORDINGS_DIR;
    };

    wlr-which-key-menu = lib.mkOption {
      description = "Name of the wlr-which-key menu";
      type = lib.types.str;
      default = "screen-recorder";
    };

    screen-audio     = mkAppOpt ''${screen-recorder} screen-audio "${cfg.directory}"'';
    screen-no-audio  = mkAppOpt ''${screen-recorder} screen-no-audio "${cfg.directory}"'';
    region-audio     = mkAppOpt ''${screen-recorder} region-audio "${cfg.directory}"'';
    region-no-audio  = mkAppOpt ''${screen-recorder} region-no-audio "${cfg.directory}"'';
    stop             = mkAppOpt ''${screen-recorder} stop'';
  };

  config = {
   assertions = [ (lib.hm.assertions.assertPlatform "custom.programs.screen-recorder" pkgs lib.platforms.linux) ];

    home.packages = [
      pkgs.wl-screenrec
      self.pkgs.screen-recorder
      (pkgs.makeDesktopItem {
        name = "screen-recoder-menu";
        desktopName = "Screen Recording";
        icon = mkIcon "screen-recorder" "󰑋";
        exec = lib.getExe (self.lib.builders.writeDmenuApplication pkgs {
          name = "screen-recorder-menu";
          entries = lib.map (e: { inherit (e) exec; label = "${e.symbol}     ${e.label}"; }) screenRecordingActions;
        });
        actions = let
          toAction = b: nameValuePair b.id {
            name = b.label;
            icon = mkIcon b.id b.symbol;
            exec = b.exec;
          };
        in listToAttrs (lib.map toAction screenRecordingActions);
      })
    ];

    # Limitation on the yaml generation that breaks the file if the line gets long (the full exe + arg)
    custom.programs.wlr-which-key.menus."${cfg.wlr-which-key-menu}" = lib.mkIf config.custom.programs.wlr-which-key.enable {
      "Ctrl+s" = cmd "Save current recording" cfg.stop;
      s = submenu "Screen" {
        a = cmd "with audio"  ''screen-recorder screen-audio "${cfg.directory}"'';
        m = cmd "no audio"    ''screen-recorder screen-no-audio "${cfg.directory}"'';
      };
      r = submenu "Region" {
        a = cmd "with audio"  ''screen-recorder region-audio "${cfg.directory}"'';
        m = cmd "no audio"    ''screen-recorder region-no-audio "${cfg.directory}"'';
      };
    };
  };
}
