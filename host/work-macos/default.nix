{ config, pkgs, lib, ... }:

# TODO: Explore https://github.com/jarun/nnn

let username = "brunohenriques";
in
{
  imports = [../../darwin];
  users.users."${username}".home  = "/Users/${username}";
  system.desktop.picture = ./wallpaper.png; # From simpledesktops

  homebrew = {
    taps = [
      "homebrew/cask"
    ];

    casks = [
      "google-chrome" # Google Meet.
      "slack"         # The usual rabbit-hole of channels.
    ];
  };

  home-manager.users."${username}" = {
    imports = [
      ../../home
      ../../home/config/sketchybar
      ../../home/config/amethyst
    ];

    # Consider moving some of these packages to project's shell.nix if team's okay with that.
    home.packages = with pkgs; [
      # Cloud Providers
      google-cloud-sdk
      awscli2

      # Kubernetes
      kubectl
      kubelogin-oidc

      # Security
      nodePackages.snyk

      # Infra
      terraform
    ];
  };

  system.stateVersion = 4;
}
