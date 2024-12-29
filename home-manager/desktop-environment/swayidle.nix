{ pkgs, lib, ... }:
{
  # https://gitlab.com/scientiac/einstein.nixos/-/blob/main/home/niriwm/locker.nix?ref_type=heads
  # https://github.com/Misterio77/nix-config/blob/main/home/gabriel/features/desktop/common/wayland-wm/swayidle.nix
  # https://gitlab.com/scientiac/einstein.nixos/-/blob/main/home/niriwm/scripts/locker.nix?ref_type=heads
  # logout: https://github.com/prescientmoon/everything-nix/blob/develop/home/features/wayland/wlogout.nix
  services.swayidle = {
    enable = true;
    timeouts = [
      { timeout = 60 * 5;   command = "${lib.getExe pkgs.niri} msg action power-off-monitors"; }
      { timeout = 60 * 10;  command = "systemctl suspend"; }
    ];
  };
}

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
