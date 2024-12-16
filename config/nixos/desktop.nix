{ pkgs, ... }:
{
  environment.systemPackages = [
    pkgs.nautilus   # File Browser
    pkgs.cheese  # Webcam
  ];

  services = {
    # TODO: https://github.com/ners/trilby/blob/7dd41d0704ebf75f8f705da066184f5ed6168441/modules/home/dconf.nix#L44
    gnome.sushi.enable = true;   # Nautilus: previews
    gvfs.enable = true;          # Nautilus: Mount, trash, and other functionalities
    flatpak.enable = true;  # Easier to run some programs. Setup afterwards: flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
  };

  programs.partition-manager.enable = true;
}
