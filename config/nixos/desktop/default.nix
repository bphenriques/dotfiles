{ pkgs, self, config, ... }:
{
  imports = [
    ./greetd.nix        # Login manager
    ./audio.nix
    ./wayland.nix       # Base wayland settings
    ./docker.nix        # Virtualization
    ./steam.nix         # Gaming: store + library
    ./gamemode.nix      # Gaming: nice to have.
    ./gamescope.nix     # Gaming: Micro-compositor
    ./nautilus.nix      # Ideally I wish it was terminal based but xdg-desktop-portal-termfilechooser seems too old to consider
    # ./sunshine.nix    # Still doesnt work.
  ];

  # Boot
  boot.kernelParams = [
    "boot.shell_on_fail"  # Use shell when booting fails
  ];

  services.flatpak.enable = true; # Easier to run some programs. Setup afterwards: flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo

  environment.systemPackages = [
    pkgs.libnotify  # Notifications

    # `top` but for GPUs. Very very useful to see which GPU is being used
    (pkgs.nvtopPackages.amd.override { nvidia = (builtins.elem "nvidia" config.services.xserver.videoDrivers); })
  ];
}
