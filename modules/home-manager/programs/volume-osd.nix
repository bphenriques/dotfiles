{ lib, pkgs, config, self, ... }:
let
  cfg = config.custom.programs.volume-osd;

  sinkDeviceOpt = lib.types.submodule {
    options = {
      label = lib.mkOption { type = lib.types.str; };
      name = lib.mkOption { type = lib.types.str; };
    };
  };

  cmd = desc: cmd: { inherit desc cmd; };
  cmdKeepOpen = desc: cmd: { inherit desc cmd; keep_open = true; };
  submenu = desc: submenu: { inherit desc submenu; };

  cmdMoveSink = sink: cmdKeepOpen "Output: ${sink.label}" "volume-osd sink-move ${sink.name}";
in
{
  options.custom.programs.volume-osd = {
    enable = lib.mkEnableOption "custom-volume-osd";
    package = lib.mkOption {
      type = lib.types.package;
      default = self.pkgs.volume-osd;
    };

    devices = lib.mkOption {
      type = lib.types.listOf sinkDeviceOpt;
      default = [ ];
    };

    defaultSinks = {
      internal = lib.mkOption { type = lib.types.nullOr sinkDeviceOpt; default = null; };
      external = lib.mkOption { type = lib.types.nullOr sinkDeviceOpt; default = null; };
      headphones = lib.mkOption { type = lib.types.nullOr sinkDeviceOpt; default = null; };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.programs.custom-volume-osd" pkgs lib.platforms.linux) ];
    custom.programs.wlr-which-key.menus.volume-osd = {
      c = cmd "Configure" "pavucontrol";
      m = cmdKeepOpen "Toggle mute" "volume-osd sink-toggle-mute";
      s = cmdKeepOpen "Set next audio speaker" "volume-osd sink-move-next";
      i = lib.mkIf (cfg.defaultSinks.internal != null) (cmdMoveSink cfg.defaultSinks.internal);
      e = lib.mkIf (cfg.defaultSinks.external != null) (cmdMoveSink cfg.defaultSinks.external);
      h = lib.mkIf (cfg.defaultSinks.headphones != null) (cmdMoveSink cfg.defaultSinks.headphones);
    };

    home.packages = [
      cfg.package
      pkgs.pavucontrol      # UI
      pkgs.pulseaudio       # Command line
    ];
  };
}