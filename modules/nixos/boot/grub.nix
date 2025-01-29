{ pkgs, lib, config, ... }:
let
  cfg = config.custom.boot.grub;
in {
  options.custom.boot.grub = with lib.types; {
    enable = lib.mkEnableOption "custom-grub" // {
      default = config.boot.loader.grub.enable;
    };

    windowsEfiDevice = lib.mkOption {
      type = lib.types.nullOr str;
      description = "Device where the Windows EFY System is. Get using `sudo fdisk -l` followed by `sudo blkid {device}` to get the UUID field.";
      default = null;
    };

    windowsEntryLabel = lib.mkOption {
      type = str;
      description = "The name of the grub menu entry to boot to Windows";
      default = "Windows";
    };

    fontSize = lib.mkOption {
      type = int;
      description = "Font size. Adjust to the screen as it is not DPI aware.";
      default = 36;
    };

    timeout = lib.mkOption {
      type = int;
      description = "Grub boot selection timeout. Set to 0 for the impatient (press ESC to enter the screen)";
      default = 0;
    };
  };

  config = lib.mkIf cfg.enable {
    boot.loader = {
      timeout = 0;  # Shorten the bootup time. Press ESC to go to the boot menu
      grub = {
        font = "${pkgs.nerd-fonts.hack}/share/fonts/truetype/NerdFonts/HackNerdFontMono-Regular.ttf";
        fontSize = cfg.fontSize;
        splashImage = null;       # Blends nicely in a OLED screen.
        timeoutStyle = "hidden";  # Ensure nothing shows up regarding grub unless prompted
        extraEntries = ''
          ${lib.optionalString (cfg.windowsEfiDevice != null) ''
            menuentry "${cfg.windowsEntryLabel}" {
              search --fs-uuid --no-floppy --set=root ${cfg.windowsEfiDevice}
              chainloader (''${root})/EFI/Microsoft/Boot/bootmgfw.efi
            }
            ''
          }

          menuentry "BIOS Setup" --class efi {
            fwsetup
          }
          menuentry "Reboot" --class restart {
            reboot
          }
          menuentry "Shutdown" --class shutdown {
            halt
          }
        '';
      };
    };

    environment.systemPackages = lib.optionals (cfg.windowsEfiDevice != null) [
      (pkgs.writeScriptBin "reboot-to-windows" ''sudo grub-reboot "${cfg.windowsEntryLabel}" && reboot $@'')
    ];
  };
}

