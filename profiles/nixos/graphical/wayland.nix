{ pkgs, ... }: {
  programs.niri.enable = true; # Already handles gnome keyring, xdg-portal and other required stuff in: https://github.com/YaLTeR/niri/wiki/Important-Software#portals
  security.soteria.enable = true; # niri ships no polkit agent, so admin prompts from apps would fail
  environment.systemPackages = [
    pkgs.wl-clipboard
  ];
}