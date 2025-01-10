{ pkgs, lib, self, ... }:
{
  # https://gitlab.com/scientiac/einstein.nixos/-/blob/main/home/niriwm/locker.nix?ref_type=heads
  # https://github.com/Misterio77/nix-config/blob/main/home/gabriel/features/desktop/common/wayland-wm/swayidle.nix
  # logout: https://github.com/prescientmoon/everything-nix/blob/develop/home/features/wayland/wlogout.nix
  services.swayidle = {
    enable = true;
    timeouts = [
      {
        timeout = 60 * 1;
        command = "${lib.getExe self.pkgs.osd-brightness} dim";
        resumeCommand = "${lib.getExe self.pkgs.osd-brightness} restore";
      }
      {
        timeout = 60 * 5;
        command = "${lib.getExe pkgs.niri} niri msg action power-off-monitors"; # niri restores the active monitors oob
        # TODO: Lock? loginctl lock-session
      }
      {
        timeout = 60 * 10;
        command = "systemctl suspend";
      }
    ];
  };
}

# TODO: RGB keyboard if applicable

# https://github.com/swaywm/swayidle/blob/master/swayidle.1.scd
# https://github.com/nix-community/home-manager/blob/master/modules/services/swayidle.nix


/*
  cst = "${./chisato.jpg}";
  cst-blurred = pkgs.runCommand "chisato.jpg" {
    nativeBuildInputs = with pkgs;[ imagemagick ];
  } ''convert -blur 14x5 ${cst} $out'';
  programs.swaylock.settings = {
    show-failed-attempts = true;
    daemonize = true;
    image = "${cst-blurred}";
    scaling = "fill";
  };
*/
