{ pkgs, ... }:
{
  imports = [
    ./steam.nix       # Gaming
    # ./sunshine.nix # TODO: does not work
  ];

  # Boot
  custom.boot.plymouth.enable = true;
  boot.kernelParams = [
    "boot.shell_on_fail"  # Use shell when booting fails
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
    extraConfig.pipewire."92-low-latency" = {
      "context.properties" = {
        "default.clock.rate" = 44100;
        "default.clock.quantum" = 512;
        "default.clock.min-quantum" = 512;
        "default.clock.max-quantum" = 512;
      };
    };
  };
  security.rtkit.enable = true;        # Recommended for pipewire
  services.pulseaudio.enable = false;  # Disable PulseAudio as pipewire is preferable

  services = {
    # TODO: https://github.com/ners/trilby/blob/7dd41d0704ebf75f8f705da066184f5ed6168441/modules/home/dconf.nix#L44
    flatpak.enable = true;        # Easier to run some programs. Setup afterwards: flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
  };

  # Other
  # TODO: https://github.com/Aylur/dotfiles/blob/main/nixos/system.nix#L44
  environment.systemPackages = with pkgs; [
    konsole   # Backup terminal in case something goes wrong

    # Personalization
    morewaita-icon-theme
    adwaita-icon-theme
    qogir-icon-theme
  ];
}
