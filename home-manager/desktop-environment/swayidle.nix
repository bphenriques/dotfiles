{ pkgs, lib, ... }:
{
  services.swayidle = {
    enable = true;
    timeouts = [
      {
        timeout = 60 * 10;
        command = "${lib.getExe pkgs.niri} msg action power-off-monitors";
      }
      {
        timeout = 600;
        command = "${lib.getExe pkgs.swaylock-effects} --screenshots --clock --indicator --indicator-radius 100 --indicator-thickness 7 --effect-blur 7x5 --effect-vignette 0.5:0.5 --ring-color f5c2e7 --text-color cdd6f4 --key-hl-color fab387 --line-color 00000000 --inside-color 1e1e2e88 --separator-color 00000000 --grace 2 --fade-in 0.2";
      }
    ];
  };
}