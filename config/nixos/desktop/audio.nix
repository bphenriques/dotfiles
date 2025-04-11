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
      # Default rate is 48kHz which is fine but can avoid resampling if the rate is natively supported:
      # See list of rates supported: `grep -E 'Codec|Audio Output|rates' /proc/asound/card*/codec#*`
      # You can see the current rate using 'pw-top'
      "91-clock-rate"."context.properties" = {
        "default.clock.rate" = 48000;
        "default.clock.allowed-rate" = [ 32000 44100 48000 88200 96000 ];
      };

      # Fixes stutters while gaming at the cost of slight latency. Won't pretend I fully understand (yet).
      # Preview changes in runtime: `pw-metadata -n settings 0 clock.force-quantum 2048`
      "92-audio-glitches"."context.properties" = {
        "default.clock.quantum" = 1024;
        "default.clock.min-quantum" = 512;
        "default.clock.max-quantum" = 2048;
      };
    };
  };
  security.rtkit.enable = true;        # Recommended for pipewire when gaming (realtime)
}
