{ pkgs, lib, config, ... }:

let
  cfg = config.custom.system.boot-theme;
in
{
  options.custom.system.boot-theme = {
    enable = lib.mkEnableOption "boot-theme";
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

      # Theme:
      # - Pure black to take advantage of the OLED screen.
      # - Font big enough to read without squinting the eyes.
      # - Hide GRUB unless I keep ESC pressed. This allows a smoother transition to the plymouth animation.
      loader.timeout = 0;
      loader.grub = lib.mkIf config.boot.loader.grub.enable {
        timeoutStyle = "hidden";
        font = "${pkgs.nerdfonts}/share/fonts/truetype/NerdFonts/HackNerdFontMono-Regular.ttf";
        fontSize = 48;
        backgroundColor = "#000000";
        splashImage = null;
      };
    };
  };
}
