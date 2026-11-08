{ lib, pkgs, config, osConfig, self, ... }:
let
  cfg = config.my.programs.volume-osd;

  volume-osd = lib.getExe cfg.package;
  volume = device: [ "${volume-osd} ${device}-decrease" "${volume-osd} ${device}-increase" ];
  mkIcon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; };

  glyphs = {
    headphones = "󰋋";
    headset = "󰋎";
    internal = "󰽟";
    external = "󰓃";
    microphone = "󰍬";
    microphoneOff = "󰍭";
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
  options.my.programs.volume-osd = {
    enable = lib.mkEnableOption "custom-volume-osd";
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
      default = self.packages.volume-osd.override {
        headphonesIcon = mkIcon "volume-osd-headphones" glyphs.headphones;
        headsetIcon = mkIcon "volume-osd-headset" glyphs.headset;
        internalSpeakersIcon = mkIcon "volume-osd-internal" glyphs.internal;
        externalSpeakersIcon = mkIcon "volume-osd-external" glyphs.external;
        microphoneIcon = mkIcon "volume-osd-micro" glyphs.microphone;
        errorIcon = mkIcon "volume-osd-error-icon" "󰀨";
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

    my.programs.desktop-shell.settings = {
      osd.adjust = {
        sink = volume "sink";
        source = volume "source";
      };
      commands = {
        audioDevice = "${volume-osd} sink-status";
        audioDevices = "${volume-osd} devices";
        audioSelect = "${volume-osd} select";
      };
    };

    my.programs.desktop-shell.menu.entries = lib.mkOrder 100 [
      { key = "a"; desc = "Audio output"; status = "sink"; list = "sinks"; pins = menuPins cfg.pins.sinks; submenu = [
        { key = [ "Down" "Up" ];    desc = "Volume";          cmd = volume "sink"; keepOpen = true; }
        { key = "m";              desc = "Mute";            cmd = "${volume-osd} sink-toggle-mute"; keepOpen = true; }
        { key = "space";          desc = "Choose device...";  cmd = "${volume-osd} sink-move-fuzzel"; }
        { key = "c";              desc = "Configure...";      cmd = "${lib.getExe pkgs.pavucontrol} --tab=3"; }
      ]; }

      { key = "A"; desc = "Audio input"; status = "source"; list = "sources"; pins = menuPins cfg.pins.sources; submenu = [
        { key = [ "Down" "Up" ];    desc = "Volume";          cmd = volume "source"; keepOpen = true; }
        { key = "m";              desc = "Mute";            cmd = "${volume-osd} source-toggle-mute"; keepOpen = true; }
        { key = "space";          desc = "Choose device...";  cmd = "${volume-osd} source-move-fuzzel"; }
        { key = "c";              desc = "Configure...";      cmd = "${lib.getExe pkgs.pavucontrol} --tab=4"; }
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
