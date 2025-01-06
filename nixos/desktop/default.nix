{ pkgs, ... }:
{
  imports = [
    ./steam.nix
    # ./sunshine.nix # TODO: does not work
  ];

  # Graphics
  hardware = {
    graphics.enable = true;
    graphics.enable32Bit = true;
  };

  # Audio
  services.pipewire = {
    enable = true;                     # Enable pipewire
    alsa.enable = true;                # Improve compatibility
    alsa.support32Bit = true;          # Improve compatibility
    pulse.enable = true;               # Pulse audio emulation to improve compatibility
    wireplumber.enable = true;         # Audio routing policy
  };
  security.rtkit.enable = true;        # Recommended for pipewire
  services.pulseaudio.enable = false;  # Disable PulseAudio as pipewire is preferable

  services = {
    # TODO: https://github.com/ners/trilby/blob/7dd41d0704ebf75f8f705da066184f5ed6168441/modules/home/dconf.nix#L44
    flatpak.enable = true;        # Easier to run some programs. Setup afterwards: flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
  };
}
