{ lib, pkgs, config, self, ... }:
let
  inherit (builtins) listToAttrs;
  inherit (lib) nameValuePair;

  mkIcon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; };

  package = self.packages.screen-recorder.override {
    recordIcon = mkIcon "screen-recorder-record-icon" "󰑋";
    informationIcon = mkIcon "screen-recorder-information-icon" "";
    errorIcon = mkIcon "screen-recorder-error-icon" "";
    audioDevice = "recording.monitor";  # desktop + mic mix, profiles/nixos/graphical/audio.nix
  };
  screen-recorder = lib.getExe package;
  directory = config.xdg.userDirs.extraConfig.RECORDINGS;

  screenRecordingActions = [
    { id = "recording-screen"; symbol = "󰹑"; label = "Screen"; exec = ''${screen-recorder} screen "${directory}"''; }
    { id = "recording-region"; symbol = ""; label = "Region"; exec = ''${screen-recorder} region "${directory}"''; }
  ];
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  home.packages = [
    package
    (pkgs.makeDesktopItem {
      name = "screen-recorder";
      desktopName = "Screen Recording";
      icon = mkIcon "screen-recorder" "󰑋";
      exec = ''${screen-recorder} screen "${directory}"'';
      actions = let
        toAction = b: nameValuePair b.id {
          name = b.label;
          icon = mkIcon b.id b.symbol;
          inherit (b) exec;
        };
      in listToAttrs (lib.map toAction screenRecordingActions);
    })
  ];

  my.programs.niri.bindings = {
    "Ctrl+Print"       = ''spawn "${screen-recorder}" "screen" "${directory}"'';
    "Ctrl+Shift+Print" = ''spawn "${screen-recorder}" "region" "${directory}"'';
  };
}
