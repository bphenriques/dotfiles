{ pkgs, lib, config, ... }:

let
  cfg = config.custom.system.graphical-boot;
in
{
  options.custom.system.graphical-boot = {
    enable = lib.mkEnableOption "graphical-boot";
    theme = lib.mkOption {
      type = lib.types.enum [ "sphere" "angular" ];
      description = "One of the boot animations available in https://github.com/adi1090x/plymouth-themes";
      default = "sphere";
    };
  };

  config = lib.mkIf cfg.enable {
    boot = {
      plymouth = {
        enable = true;
        theme = cfg.theme;
        themePackages = with pkgs; [
          (adi1090x-plymouth-themes.override {
            selected_themes = [ cfg.theme ];
          })
        ];
      };

      # Silent boot
      consoleLogLevel = 0;
      initrd.verbose = false;
      kernelParams = [
        "quiet"
        "splash"
        "boot.shell_on_fail"
        "loglevel=3"
        "rd.systemd.show_status=false"
        "rd.udev.log_level=3"
        "udev.log_priority=3"
      ];

      # Hide OS choice unless ESC is held down during that time.
      loader.timeout = 0;
      loader.grub = lib.mkIf config.boot.loader.grub.enable {
        timeoutStyle = "hidden";  # Hide menu
        #splashImage = null;       # Force full-text mode. A image appears otherwise, breaking the immersion.
      };
    };
  };
}
