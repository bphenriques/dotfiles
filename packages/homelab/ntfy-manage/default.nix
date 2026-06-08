{ lib, pkgs, ... }:
let
  script = pkgs.writeTextFile {
    name = "ntfy-manage.nu";
    text = lib.fileContents ./script.nu;
  };
in
pkgs.writeShellApplication {
  name = "ntfy-manage";
  runtimeInputs = with pkgs; [ nushell coreutils ntfy-sh ];
  text = ''
    exec nu ${script} "$@"
  '';
  meta.platforms = lib.platforms.linux;
}
