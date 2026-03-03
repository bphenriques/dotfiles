{ pkgs, ... }: {
  programs.niri.enable = true; # Already handles gnome keyring, xdg-portal and other required stuff in: https://github.com/YaLTeR/niri/wiki/Important-Software#portals
  environment.systemPackages = [
    pkgs.wl-clipboard
  ];
}