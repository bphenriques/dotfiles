{ lib, pkgs, config, osConfig, self, ... }:
let
  cfg = config.my.programs.audio;

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
in
{
  options.my.programs.audio = {
    enable = lib.mkEnableOption "PipeWire audio tools";
    pins = {
      sinks = lib.mkOption {
        type = lib.types.listOf pin;
        default = [ ];
        description = "Known outputs, in the order menus list them";
      };
      sources = lib.mkOption {
        type = lib.types.listOf pin;
        default = [ ];
        description = "Known inputs, in the order menus list them";
      };
    };

    glyphs = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      readOnly = true;
      default = glyphs;
      description = "Glyph by device type, and microphoneOff, for other audio displays";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      (lib.hm.assertions.assertPlatform "my.programs.audio" pkgs lib.platforms.linux)
      { assertion = osConfig.services.pipewire.enable; message = "Requires pipewire enabled in the system."; }
      { assertion = osConfig.services.pipewire.pulse.enable; message = "Requires pipewire-pulse enabled in the system."; }
      { assertion = osConfig.services.pipewire.wireplumber.enable; message = "Requires pipewire wireplumber enabled in the system."; }
    ];

    home.packages = [
      # Using pulseaudio tools without actually using pulseaudio
      pkgs.pavucontrol  # UI
      pkgs.pipewire     # Preferred command line interface
      pkgs.pulseaudio   # Other command line interface
    ];
  };
}
