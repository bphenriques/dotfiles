{ lib, pkgs, config, osConfig, self, ... }:
let
  cfg = config.my.programs.volume-osd;

  volume-osd = lib.getExe cfg.package;
  mkIcon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; };

  glyphs = {
    headphones = "󰋋";
    headset = "󰋎";
    internal = "󰽟";
    external = "󰓃";
    microphone = "󰍬";
    microphoneOff = "󰍭";
  };
in
{
  options.my.programs.volume-osd = {
    enable = lib.mkEnableOption "custom-volume-osd";
    package = lib.mkOption {
      type = lib.types.package;
      default = self.packages.volume-osd.override {
        headphonesIcon = mkIcon "volume-osd-headphones" glyphs.headphones;
        headphonesMuteIcon = mkIcon "volume-osd-headphones" "󰋐";
        headsetIcon = mkIcon "volume-osd-headset" glyphs.headset;
        headsetMuteIcon = mkIcon "volume-osd-headset-mute" "󰋐";
        internalSpeakersIcon = mkIcon "volume-osd-internal" glyphs.internal;
        internalSpeakersMuteIcon = mkIcon "volume-osd-internal-mute" "󰽠";
        externalSpeakersIcon = mkIcon "volume-osd-external" glyphs.external;
        externalSpeakersMuteIcon = mkIcon "volume-osd-external-mute" "󰓄";
        microphoneIcon = mkIcon "volume-osd-micro" glyphs.microphone;
        microphoneMuteIcon = mkIcon "volume-osd-micro-mute" glyphs.microphoneOff;
        errorIcon = mkIcon "volume-osd-error-icon" "󰀨";
        headphonesGlyph = glyphs.headphones;
        headsetGlyph = glyphs.headset;
        internalSpeakersGlyph = glyphs.internal;
        externalSpeakersGlyph = glyphs.external;
        microphoneGlyph = glyphs.microphone;
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      (lib.hm.assertions.assertPlatform "my.programs.volume-osd" pkgs lib.platforms.linux)
      { assertion = osConfig.services.pipewire.enable; message = "Requires pipewire enabled in the system."; }
      { assertion = osConfig.services.pipewire.pulse.enable; message = "Requires pipewire-pulse enabled in the system."; }
      { assertion = osConfig.services.pipewire.wireplumber.enable; message = "Requires pipewire wireplumber enabled in the system."; }
    ];

    my.programs.desktop-shell.settings.glyphs.audio = glyphs;

    my.programs.desktop-shell.menu.entries = lib.mkOrder 200 [
      { key = "a"; desc = "Audio Output"; status = "sink"; submenu = [
        { key = "Up";             desc = "Increase volume"; cmd = "${volume-osd} sink-increase";    keepOpen = true; }
        { key = "Down";           desc = "Reduce volume";   cmd = "${volume-osd} sink-decrease";    keepOpen = true; }
        { key = "Left";           desc = "Previous device"; cmd = "${volume-osd} sink-move-prev";   keepOpen = true; }
        { key = "m";              desc = "Mute";            cmd = "${volume-osd} sink-toggle-mute"; keepOpen = true; }
        { key = "space";          desc = "Select device";   cmd = "${volume-osd} sink-move-fuzzel"; }
        { key = "Right";          desc = "Next device";     cmd = "${volume-osd} sink-move-next";   keepOpen = true; }
        { key = "c";              desc = "Configure";       cmd = "${lib.getExe pkgs.pavucontrol} --tab=3"; }
      ]; }

      { key = "A"; desc = "Audio Input"; status = "source"; submenu = [
        { key = "Up";             desc = "Increase volume"; cmd = "${volume-osd} source-increase";    keepOpen = true; }
        { key = "Down";           desc = "Reduce volume";   cmd = "${volume-osd} source-decrease";    keepOpen = true; }
        { key = "m";              desc = "Mute";            cmd = "${volume-osd} source-toggle-mute"; keepOpen = true; }
        { key = "Left";           desc = "Previous device"; cmd = "${volume-osd} source-move-prev";   keepOpen = true; }
        { key = "Right";          desc = "Next device";     cmd = "${volume-osd} source-move-next";   keepOpen = true; }
        { key = "space";          desc = "Select device";   cmd = "${volume-osd} source-move-fuzzel"; }
        { key = "c";              desc = "Configure";       cmd = "${lib.getExe pkgs.pavucontrol} --tab=4"; }
      ]; }
    ];

    home.packages = [
      cfg.package

      # Using pulseaudio tools without actually using pulseaudio
      pkgs.pavucontrol  # UI
      pkgs.pipewire     # Preferred command line interface
      pkgs.pulseaudio   # Other command line interface
    ];
  };
}
