{ config, lib, pkgs, ... }:
let
  inherit (config.my.programs.session) actions;
  key = "q";
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  my.programs.session.exec.menu = "${config.programs.quickkey.exec.ipc} menu toggleEntry ${key}";

  programs.quickkey.settings.menu.entries = lib.mkOrder 2000 [
    { inherit key; desc = "Session"; width = 640; icon = (lib.findFirst (a: a.id == "session-shutdown") null actions).symbol; submenu = map (a: { inherit (a) key; desc = a.label; cmd = a.exec; icon = a.symbol; }) actions; }
  ];
}
