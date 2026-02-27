{ pkgs, config, ... }:
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

  networking.networkmanager.enable = true;    # More complete package to manage connectivity. Suitable for desktop.

  # Ensure we have at least 5GiB always available. Less than that and my system gets unstable during builds.
  nix.extraOptions = "min-free = ${toString (5 * 1024 * 1024 * 1024)}";

  services.flatpak.enable = true;         # Easier to run some programs. Setup afterwards: flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo

  environment.systemPackages = [
    pkgs.libnotify  # Notifications

    # `top` but for GPUs. Very very useful to see which GPU is being used
    (pkgs.nvtopPackages.amd.override { nvidia = (builtins.elem "nvidia" config.services.xserver.videoDrivers); })
  ];
}
