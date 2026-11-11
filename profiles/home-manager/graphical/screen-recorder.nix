{ lib, pkgs, config, self, ... }:
let
  inherit (builtins) listToAttrs;
  inherit (lib) nameValuePair;

  mkIcon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; };

  package = self.packages.screen-recorder.override {
    informationIcon = mkIcon "screen-recorder-information-icon" self.lib.glyphs."md-information";
    errorIcon = mkIcon "screen-recorder-error-icon" self.lib.glyphs."md-alert_circle";
    audioDevice = "recording.monitor";  # desktop + mic mix, profiles/nixos/graphical/audio.nix
  };
  screen-recorder = lib.getExe package;
  directory = config.xdg.userDirs.extraConfig.RECORDINGS;
  # Under $XDG_RUNTIME_DIR, where screen-recorder writes the start time while recording.
  stateFile = "screen-recorder";

  screenRecordingActions = [
    { id = "recording-screen"; symbol = self.lib.glyphs."md-monitor_screenshot"; label = "Screen"; exec = ''${screen-recorder} screen "${directory}"''; }
    { id = "recording-region"; symbol = self.lib.glyphs."md-selection_drag"; label = "Region"; exec = ''${screen-recorder} region "${directory}"''; }
  ];
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  home.packages = [
    package
    (pkgs.makeDesktopItem {
      name = "screen-recorder";
      desktopName = "Screen Recording";
      icon = mkIcon "screen-recorder" self.lib.glyphs."md-record_rec";
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

  # The recording state file must exist up front for Quickshell to watch it.
  systemd.user.tmpfiles.rules = [ "f %t/${stateFile} 0600 - - -" ];

  my.programs.desktop-shell.integrations.recording = {
    settings = { file = stateFile; stop = "${screen-recorder} stop"; };
  };

  my.programs.niri.bindings = {
    "Ctrl+Print"       = ''spawn "${screen-recorder}" "screen" "${directory}"'';
    "Ctrl+Shift+Print" = ''spawn "${screen-recorder}" "region" "${directory}"'';
  };
}
