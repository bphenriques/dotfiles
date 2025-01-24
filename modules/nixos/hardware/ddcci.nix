{ pkgs, config, lib, self, ... }:
with lib;

# https://github.com/ejmastnak/ejmastnak.com/blob/40e0d20bceedc75bc2111201976bb30bc421577f/content/tutorials/arch/monitor-hotplug.md
# https://github.com/sweenu/nixfiles/blob/77a35153c18ccfa9975c8c712fc6640806a4a102/profiles/laptop.nix#L27
# https://github.com/sweenu/nixfiles/blob/77a35153c18ccfa9975c8c712fc6640806a4a102/profiles/laptop.nix#L27
# https://github.com/floscr/dotfiles/blob/main/new/modules/services/hotplug.nix#L30
# https://discourse.nixos.org/t/brightness-control-of-external-monitors-with-ddcci-backlight/8639/17
# https://discourse.nixos.org/t/ddcci-kernel-driver/22186/7
let
  cfg = config.custom.hardware.ddcci;
in {
  options.custom.hardware.ddcci = with types; {
    enable = mkEnableOption "Manage external monitors through `/sys/class/backlight/`. User needs to belong to 'i2c' group";
  };

  config = lib.mkIf cfg.enable {
    hardware.i2c.enable = true;
    boot.extraModulePackages = [ config.boot.kernelPackages.ddcci-driver ];
    boot.kernelModules = [ "ddcci" ];
    #services.udev.extraRules = ''
    #  KERNEL=="card0", SUBSYSTEM=="drm", ACTION=="change", RUN+="${lib.getExe self.pkgs.ddcci-external-screen-hotplug}"
    #'';

    # TODO: Limitation, does not work when we plug/unplug devices. Works for me.
    systemd.services."init-ddcci-connected-monitors" = {
       wantedBy = [ "graphical.service" ];
       after = [ "graphical.service" ];
       serviceConfig = {
         Type = "oneshot";
         RemainAfterExit = false;
         ExecStart = ''${lib.getExe self.pkgs.ddcci-util} init"'';
       };
    };
    # Quick enough that does not justify modelling as a service
    #systemd.services.graphical.postStart = ''
    #  ${pkgs.kmod}/bin/modprobe -r ddcci && ${pkgs.kmod}/bin/modprobe ddcci
    #'';

    # Temporary
    environment.systemPackages = with pkgs; [
      brightnessctl
      ddcutil
    ];
  };
}
