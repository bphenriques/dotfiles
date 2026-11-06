{ config, lib, pkgs, ... }:
let
  colors = config.lib.stylix.colors.withHashtag;

  theme = pkgs.writeText "Theme.qml" ''
    pragma Singleton

    import QtQuick
    import Quickshell

    Singleton {
      readonly property color background: "${colors.base00}"
      readonly property color foreground: "${colors.base05}"
      readonly property color accent: "${colors.base08}"
      readonly property string monospaceFont: "${config.stylix.fonts.monospace.name}"
    }
  '';

  shell = pkgs.runCommand "quickshell-config" { } ''
    cp -r ${./config} $out
    chmod u+w $out
    cp ${theme} $out/Theme.qml
  '';
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  programs.quickshell = {
    enable = true;
    systemd.enable = true;
    activeConfig = "shell";
    configs.shell = shell;
  };

  systemd.user.services.quickshell.Unit.X-Restart-Triggers = [ "${shell}" ];

  my.programs.niri.layerRules.screenCapture.block = [ ''namespace="^recording-indicator$"'' ];
}
