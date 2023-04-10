{ config, pkgs, lib, ... }:

let username = "brunohenriques";
in
{
  imports = [../../darwin];
  users.users."${username}".home  = "/Users/${username}";
  system.desktop.picture = ./wallpaper.png; # From simpledesktops

  homebrew = {
    taps = [
      "homebrew/cask"
      "snyk/tap"
      "int128/kubelogin"
    ];

    brews = [
      "kubectl"
      "awscli"
      "snyk"                        # Security.
      "int128/kubelogin/kubelogin"  # Kubernetes
    ];

    casks = [
      "google-chrome" # Google Meet.
      "slack"         # The usual rabbit-hole of channels.
    ];
  };

  home-manager.users."${username}" = {
    imports = [../../home];

    # Consider moving some of these packages to project's shell.nix if team's okay with that.
    home.packages = with pkgs; [
      google-cloud-sdk
      terraform
      kubectl
      kubelogin-oidc
    ];
  };
}
