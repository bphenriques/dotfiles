{ config, lib, pkgs, self, ... }:
let
  cfg = config.my.programs.audio;
  inherit (cfg) glyphs;
  wpctl = lib.getExe' pkgs.wireplumber "wpctl";
  default = { sink = "@DEFAULT_AUDIO_SINK@"; source = "@DEFAULT_AUDIO_SOURCE@"; };
  maxVolume = 150;   # percent, for the keys and the menu alike
  volume = kind: [ "${wpctl} set-volume ${default.${kind}} 5%-" "${wpctl} set-volume -l ${toString (maxVolume / 100.0)} ${default.${kind}} 5%+" ];
  mute = kind: "${wpctl} set-mute ${default.${kind}} toggle";
  menuPins = map (pin: { inherit (pin) match label; icon = glyphs.${pin.type}; });
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  programs.quick-key.settings.audio.max = maxVolume;

  programs.quick-key.settings.menu.entries = lib.mkOrder 100 [
    { key = "a"; desc = "Audio output"; width = 640; status = "sink"; list = "sinks"; pins = menuPins cfg.pins.sinks; submenu = [
      { key = [ "Down" "Up" ]; desc = "Output volume"; action = "sinkVolume"; }
      { key = "m"; desc = "Mute output"; action = "sinkMute"; }
      { key = "c"; desc = "Configure..."; icon = self.lib.glyphs."md-cog"; cmd = "${lib.getExe pkgs.pavucontrol} --tab=3"; }
    ]; }

    { key = "A"; desc = "Audio input"; width = 640; status = "source"; list = "sources"; pins = menuPins cfg.pins.sources; submenu = [
      { key = [ "Down" "Up" ]; desc = "Input volume"; icon = glyphs.microphone; action = "sourceVolume"; }
      { key = "m"; desc = "Mute input"; icon = glyphs.microphoneOff; action = "sourceMute"; }
      { key = "c"; desc = "Configure..."; icon = self.lib.glyphs."md-cog"; cmd = "${lib.getExe pkgs.pavucontrol} --tab=4"; }
    ]; }
  ];

  my.programs.niri.bindings = {
    "XF86AudioRaiseVolume allow-when-locked=true" = ''spawn-sh "${lib.last (volume "sink")}"'';
    "XF86AudioLowerVolume allow-when-locked=true" = ''spawn-sh "${lib.head (volume "sink")}"'';
    "XF86AudioMute allow-when-locked=true" = ''spawn-sh "${mute "sink"}"'';
    "XF86AudioMicMute allow-when-locked=true" = ''spawn-sh "${mute "source"}"'';
  };
}
