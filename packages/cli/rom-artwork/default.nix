{
  lib,
  pkgs,
  ...
}:
pkgs.writeShellApplication {
  name = "rom-artwork";
  runtimeInputs = [ pkgs.skyscraper ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.linux;
}
