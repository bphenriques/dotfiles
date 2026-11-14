{ config, lib, pkgs, self, ... }:
let
  cfg = config.my.programs.audioctl;
  inherit (cfg) glyphs;
  audioctl = lib.getExe cfg.package;
  volume = device: [ "${audioctl} ${device}-decrease" "${audioctl} ${device}-increase" ];
  menuPins = map (pin: { inherit (pin) match label; icon = glyphs.${pin.type}; });
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  programs.quickkey.settings.audio = {
    devices = "${audioctl} devices";
    select = "${audioctl} select";
    adjust = {
      sink = volume "sink";
      source = volume "source";
    };
  };

  programs.quickkey.settings.menu.entries = lib.mkOrder 100 [
    { key = "a"; desc = "Audio output"; status = "sink"; list = "sinks"; pins = menuPins cfg.pins.sinks; submenu = [
      { key = [ "Down" "Up" ]; desc = "Volume"; icon = self.lib.glyphs."md-volume_high"; cmd = volume "sink"; keepOpen = true; }
      { key = "m"; desc = "Mute"; icon = self.lib.glyphs."md-volume_off"; activeWhen = "sink-muted"; cmd = "${audioctl} sink-toggle-mute"; keepOpen = true; }
      { key = "space"; desc = "Choose device..."; icon = self.lib.glyphs."md-swap_horizontal"; cmd = "${audioctl} sink-move-fuzzel"; }
      { key = "c"; desc = "Configure..."; icon = self.lib.glyphs."md-cog"; cmd = "${lib.getExe pkgs.pavucontrol} --tab=3"; }
    ]; }

    { key = "A"; desc = "Audio input"; status = "source"; list = "sources"; pins = menuPins cfg.pins.sources; submenu = [
      { key = [ "Down" "Up" ]; desc = "Volume"; icon = glyphs.microphone; cmd = volume "source"; keepOpen = true; }
      { key = "m"; desc = "Mute"; icon = glyphs.microphoneOff; activeWhen = "source-muted"; cmd = "${audioctl} source-toggle-mute"; keepOpen = true; }
      { key = "space"; desc = "Choose device..."; icon = self.lib.glyphs."md-swap_horizontal"; cmd = "${audioctl} source-move-fuzzel"; }
      { key = "c"; desc = "Configure..."; icon = self.lib.glyphs."md-cog"; cmd = "${lib.getExe pkgs.pavucontrol} --tab=4"; }
    ]; }
  ];
}
