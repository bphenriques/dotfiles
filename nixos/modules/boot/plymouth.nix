{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.custom.boot.plymouth;
in {
  options.custom.boot.plymouth = with types; {
    enable = mkEnableOption "plymouth";
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

      # Hide logs during boot (press Escape to show them) and smoothen transition
      consoleLogLevel = 2;
      kernelParams = [
        "plymouth.use-simpledrm"
        "quiet"
        "udev.log_level=3"
        "rd.systemd.show_status=false"
        "rd.udev.log_level=3"
      ];
      loader.timeout = 0;
      loader.grub = lib.mkIf config.boot.loader.grub.enable {
        timeoutStyle = "hidden";
        splashImage = null;
      };
    };
  };
}