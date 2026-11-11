{ lib, writeShellApplication, coreutils, niri, quickshell, wtype, wlrctl, grim }:
writeShellApplication {
  name = "desktop-shell-preview";
  runtimeInputs = [ coreutils niri quickshell wtype wlrctl grim ];
  runtimeEnv.PREVIEW_NIRI_CONFIG = ./niri.kdl;
  text = builtins.readFile ./script.sh;
  meta.platforms = lib.platforms.linux;
}
