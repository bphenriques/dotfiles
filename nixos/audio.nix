{ ... }:

# Audio - Pipewire over ALSA and PulseAudio: https://nixos.wiki/wiki/PipeWire
{
  sound.enable = false;                # Disable ALSA (it is used as a low-level API for pipewire): https://nixos.wiki/wiki/ALSA
  hardware.pulseaudio.enable = false;  # Disable PulseAudio: https://nixos.wiki/wiki/PulseAudio
  security.rtkit.enable = true;        # Recommended for pipewire
  services.pipewire = {
    enable = true;                     # Enable pipewire
    alsa.enable = true;                # For better compatibility
    alsa.support32Bit = true;          # For better compatibility
    pulse.enable = true;               # For better compatibility
  };
}
