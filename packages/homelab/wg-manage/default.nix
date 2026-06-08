{ lib, pkgs, ... }:
let
  script = pkgs.writeTextFile {
    name = "wg-manage.nu";
    text = lib.fileContents ./script.nu;
  };
in
pkgs.writeShellApplication {
  name = "wg-manage-bin";
  runtimeInputs = with pkgs; [ nushell wireguard-tools qrencode coreutils ];
  text = ''
    exec nu ${script} "$@"
  '';
  meta.platforms = lib.platforms.linux;
}
