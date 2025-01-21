{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.custom.boot.plymouth;
in {
  options.custom.boot.plymouth = with types; {
    enable = mkEnableOption "plymouth" // {
      description = "Enable it to set a nice graphical boot menu. Set the boot loader to accordingly to have a nicer transition";
    };

    theme = lib.mkOption {
      type = lib.types.enum [ "angular" ];
      description = "One of the boot animations available in https://github.com/adi1090x/plymouth-themes";
      default = "angular";
    };
  };

  config = lib.mkIf cfg.enable {
    boot = {
      plymouth = {
        enable = true;
        theme = cfg.theme;
        themePackages = [
          (pkgs.adi1090x-plymouth-themes.override {
            selected_themes = [ cfg.theme ];
          })
        ];
      };

      # Silent boot (press Escape to show logs)
      consoleLogLevel = 0;
      kernelParams = [
        "plymouth.use-simpledrm"
        "quiet"
        "udev.log_level=3"
        "rd.systemd.show_status=false"
        "rd.udev.log_level=3"
      ];
    };
  };
}