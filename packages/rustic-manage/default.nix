{ lib, pkgs, ... }:
let
  script = pkgs.writeTextFile {
    name = "rustic-manage.nu";
    text = lib.fileContents ./script.nu;
  };
in
pkgs.writeShellApplication {
  name = "rustic-manage-bin";
  runtimeInputs = with pkgs; [ nushell rustic ];
  text = ''
    exec nu ${script} "$@"
  '';
  meta.platforms = lib.platforms.linux;
}
