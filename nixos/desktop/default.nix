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
  hardware.pulseaudio.enable = false;  # Disable PulseAudio: https://nixos.wiki/wiki/PulseAudio
  security.rtkit.enable = true;        # Recommended for pipewire
  services.pipewire = {
    enable = true;                     # Enable pipewire
    alsa.enable = true;                # For better compatibility
    alsa.support32Bit = true;          # For better compatibility
    pulse.enable = true;               # Pulse audio emulation for better compatibility
    wireplumber.enable = true;         # Audio routing policy if I understood correctly.
  };

  environment.systemPackages = [
    pkgs.cheese     # Webcam TODO do not include it if there is no webcam
  ];

  services = {
    # TODO: https://github.com/ners/trilby/blob/7dd41d0704ebf75f8f705da066184f5ed6168441/modules/home/dconf.nix#L44
    flatpak.enable = true;        # Easier to run some programs. Setup afterwards: flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
  };

  # FIXME s.partition-manager.enable = true;
}
