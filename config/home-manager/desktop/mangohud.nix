{ lib, pkgs, ...}:
lib.mkIf pkgs.stdenv.isLinux {
  stylix.targets.mangohud.enable = true;
  programs.mangohud = {
    enable = true;
    enableSessionWide = true;
    settings = {
      no_display = true;  # Hide until toggled
      toggle_hud = "Shift_L+F1";
      toggle_hud_position = "Shift_L+F2";
      toggle_logging = "Shift_L+F3";

      cpu_stats = true;
      cpu_temp = true;
      gpu_stats = true;
      gpu_temp = true;
      ram = true;
      vram = true;
      fps = true;
      frame_timing = false;
      hud_compact = true;
    };
  };
}