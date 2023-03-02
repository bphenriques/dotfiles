{ config, pkgs, lib, ... }:
{
  # Gpg
  programs.gpg.enable = true;
  services.gpg-agent.enable = true;

  # Fonts
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    firefox-wayland
    python3
    (nerdfonts.override { fonts = [ "Hack" ]; })
  ];

  # Firefox hacks around wayland (see https://github.com/swaywm/sway/wiki/Running-programs-natively-under-wayland#gtk3)
  home.sessionVariables = {
    MOZ_ENABLE_WAYLAND = 1;
    XDG_CURRENT_DESKTOP = "sway";
  };
}
