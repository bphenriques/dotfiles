{ lib, pkgs, ... }:
let
  script = pkgs.writeTextFile {
    name = "pocket-id-manage.nu";
    text = lib.fileContents ./script.nu;
  };
in
pkgs.writeShellApplication {
  name = "pocket-id-manage-bin";
  runtimeInputs = with pkgs; [ nushell coreutils ];
  text = ''
    exec nu ${script} "$@"
  '';
  meta.platforms = lib.platforms.linux;
}
