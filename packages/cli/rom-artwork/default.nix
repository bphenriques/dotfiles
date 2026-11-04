{
  lib,
  pkgs,
  ...
}:
pkgs.writeShellApplication {
  name = "rom-artwork";
  runtimeInputs = [ pkgs.skyscraper ];
  runtimeEnv.ARTWORK_XML = ./artwork.xml;
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.linux;
}
