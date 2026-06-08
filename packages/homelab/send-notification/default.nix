{ lib, pkgs, ... }:
let
  script = pkgs.writeTextFile {
    name = "send-notification.nu";
    text = lib.fileContents ./script.nu;
  };
in
pkgs.writeShellApplication {
  name = "send-notification";
  runtimeInputs = with pkgs; [ nushell coreutils ];
  text = ''
    exec nu ${script} "$@"
  '';
  meta.platforms = lib.platforms.linux;
}
