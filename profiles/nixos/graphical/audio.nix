_:
{
  services.pulseaudio.enable = false;  # Disable PulseAudio as pipewire is preferable
  services.pipewire = {
    enable = true;                     # Enable pipewire
    alsa.enable = true;                # Improve compatibility
    alsa.support32Bit = true;          # Improve compatibility
    pulse.enable = true;               # Pulse audio emulation to improve compatibility
    wireplumber.enable = true;         # Audio routing policy
    extraConfig.pipewire = {
      # Avoids resampling where natively supported (`grep rates /proc/asound/card*/codec#*`, current rate in pw-top).
      "91-clock-rate"."context.properties" = {
        "default.clock.rate" = 48000;
        "default.clock.allowed-rate" = [ 32000 44100 48000 88200 96000 ];
      };

      # Fixes gaming stutters for slight latency; try live with `pw-metadata -n settings 0 clock.force-quantum 2048`.
      "92-audio-glitches"."context.properties" = {
        "default.clock.quantum" = 1024;
        "default.clock.min-quantum" = 512;
        "default.clock.max-quantum" = 2048;
      };

      # Desktop + mic mix that screen-recorder captures from `recording.monitor`. Passive, so the mic only opens while recording.
      "93-recording-mix" = let
        loopback = captureProps: {
          name = "libpipewire-module-loopback";
          args = {
            "capture.props" = captureProps // { "node.passive" = true; };
            "playback.props" = {
              "target.object" = "recording";
              "node.passive" = true;
              "node.dont-fallback" = true;  # never leak into the speakers if `recording` is missing
            };
          };
        };
      in {
        "context.objects" = [{
          factory = "adapter";
          args = {
            "factory.name" = "support.null-audio-sink";
            "node.name" = "recording";
            "node.description" = "Recording mix";
            "media.class" = "Audio/Sink";
            "audio.position" = [ "FL" "FR" ];
          };
        }];
        "context.modules" = [
          (loopback { "stream.capture.sink" = true; })
          (loopback { })
        ];
      };
    };
  };
  security.rtkit.enable = true;        # Recommended for pipewire when gaming (realtime)
}
