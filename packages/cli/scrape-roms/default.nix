{
  lib,
  pkgs,
  ...
}:
pkgs.writeShellApplication {
  name = "scrape-roms";
  runtimeInputs = [ pkgs.skyscraper ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.linux;
}
