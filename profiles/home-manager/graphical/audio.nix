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

  my.quick-key.entries.audio = [
    { key = "a"; desc = "Audio output"; width = 720; columns = 1; status = "sink"; list = "sinks"; pins = menuPins cfg.pins.sinks; submenu = [
      { key = [ "Down" "Up" ]; desc = "Volume"; action = "sinkVolume"; }
      { key = "m"; desc = "Mute"; action = "sinkMute"; }
      { key = "a"; desc = "Apps..."; icon = self.lib.glyphs."md-apps"; list = "streams"; submenu = [   # a digit picks the app the keys change
        { key = [ "Left" "Right" ]; desc = "App volume"; action = "streamVolume"; }
        { key = "m"; desc = "Mute app"; action = "streamMute"; }
      ]; }
      { key = "c"; desc = "Configure..."; icon = self.lib.glyphs."md-cog"; cmd = "${lib.getExe pkgs.pavucontrol} --tab=3"; }
    ]; }

    { key = "A"; desc = "Audio input"; width = 720; columns = 1; status = "source"; list = "sources"; pins = menuPins cfg.pins.sources; submenu = [
      { key = [ "Down" "Up" ]; desc = "Volume"; icon = glyphs.microphone; action = "sourceVolume"; }
      { key = "m"; desc = "Mute"; icon = glyphs.microphoneOff; action = "sourceMute"; }
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
