{ lib, pkgs, config, osConfig, self, ... }:
let
  cfg = config.my.programs.audioctl;

  audioctl = lib.getExe cfg.package;
  volume = device: [ "${audioctl} ${device}-decrease" "${audioctl} ${device}-increase" ];
  mkIcon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; };

  glyphs = {
    headphones = self.lib.glyphs."md-headphones";
    headset = self.lib.glyphs."md-headset";
    internal = self.lib.glyphs."md-monitor_speaker";
    external = self.lib.glyphs."md-speaker";
    microphone = self.lib.glyphs."md-microphone";
    microphoneOff = self.lib.glyphs."md-microphone_off";
  };

  pin = lib.types.submodule {
    options = {
      match = lib.mkOption {
        type = lib.types.str;
        description = "Pattern on the PipeWire node name, with * for anything";
      };
      label = lib.mkOption {
        type = lib.types.str;
        description = "Name shown in the menu";
      };
      type = lib.mkOption {
        type = lib.types.enum [ "headphones" "headset" "internal" "external" "microphone" ];
        description = "Device type, for an icon that stays the same while the device is unplugged";
      };
    };
  };
  menuPins = map (pin: { inherit (pin) match label; icon = glyphs.${pin.type}; });
in
{
  options.my.programs.audioctl = {
    enable = lib.mkEnableOption "audioctl, the audio glue the desktop shell and keys call";
    pins = {
      sinks = lib.mkOption {
        type = lib.types.listOf pin;
        default = [ ];
        description = "Outputs that keep their digit in the menu";
      };
      sources = lib.mkOption {
        type = lib.types.listOf pin;
        default = [ ];
        description = "Inputs that keep their digit in the menu";
      };
    };
    package = lib.mkOption {
      type = lib.types.package;
      default = self.packages.audioctl.override {
        headphonesIcon = mkIcon "audioctl-headphones" glyphs.headphones;
        headsetIcon = mkIcon "audioctl-headset" glyphs.headset;
        internalSpeakersIcon = mkIcon "audioctl-internal" glyphs.internal;
        externalSpeakersIcon = mkIcon "audioctl-external" glyphs.external;
        microphoneIcon = mkIcon "audioctl-micro" glyphs.microphone;
        errorIcon = mkIcon "audioctl-error-icon" self.lib.glyphs."md-alert_circle";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      (lib.hm.assertions.assertPlatform "my.programs.audioctl" pkgs lib.platforms.linux)
      { assertion = osConfig.services.pipewire.enable; message = "Requires pipewire enabled in the system."; }
      { assertion = osConfig.services.pipewire.pulse.enable; message = "Requires pipewire-pulse enabled in the system."; }
      { assertion = osConfig.services.pipewire.wireplumber.enable; message = "Requires pipewire wireplumber enabled in the system."; }
    ];

    my.programs.desktop-shell.settings.glyphs.audio = glyphs;

    my.programs.desktop-shell.settings.commands = {
      adjust = {
        sink = volume "sink";
        source = volume "source";
      };
      audioDevices = "${audioctl} devices";
      audioSelect = "${audioctl} select";
    };

    my.programs.desktop-shell.menu.entries = lib.mkOrder 100 [
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

    my.programs.niri.bindings = {
      "XF86AudioRaiseVolume allow-when-locked=true" = ''spawn-sh "${audioctl} sink-increase"'';
      "XF86AudioLowerVolume allow-when-locked=true" = ''spawn-sh "${audioctl} sink-decrease"'';
      "XF86AudioMute allow-when-locked=true" = ''spawn-sh "${audioctl} sink-toggle-mute"'';
      "XF86AudioMicMute allow-when-locked=true" = ''spawn-sh "${audioctl} source-toggle-mute"'';
      "Shift+XF86AudioRaiseVolume allow-when-locked=true" = ''spawn-sh "${audioctl} sink-move-next"'';
      "Shift+XF86AudioLowerVolume allow-when-locked=true" = ''spawn-sh "${audioctl} sink-move-prev"'';
    };

    home.packages = [
      cfg.package

      # Using pulseaudio tools without actually using pulseaudio
      pkgs.pavucontrol  # UI
      pkgs.pipewire     # Preferred command line interface
      pkgs.pulseaudio   # Other command line interface
    ];
  };
}
