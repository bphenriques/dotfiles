{ config, pkgs, lib, ... }:
{
  # TODO: check https://github.com/iynaix/dotfiles/blob/main/nixos/users.nix
  users.users.bphenriques = {
    isNormalUser = true;
    initialPassword = "password";
    description = "bphenriques";
    extraGroups = [ "wheel" "networkmanager" "docker" ];
  };

  home-manager.users.bphenriques = { pkgs, ... }: {
    imports = [
      ../../home
      ../../home/plasma
    ];
    programs.plasma.workspace.wallpaper = ./wallpaper.png;

    # Gpg
    programs.gpg.enable = true;
    services.gpg-agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-gnome3;
    };

    home.packages = with pkgs; [
      killall     # Useful
    ];

    home.stateVersion = "24.05";
  };
}
