{ config, pkgs, lib, ... }:

let username = "brunohenriques";
in
{
  imports = [../../darwin];
  users.users."${username}".home  = "/Users/${username}";
  system.defaults.screencapture.location = "/Users/${username}/Pictures/screenshots";  # Avoid bloating the Desktop with screenshots.
  system.desktop.picture = ./wallpaper.png; # From simpledesktops

  homebrew = {
    taps = [
      "common-fate/granted"
      "snyk/tap"
    ];

    brews = [
      "snyk"      # Security. The NixOS package is broken in MacOS.
      "python3"   # Implicit dependency of Aiven client
      "kubeseal"  # K8s stuff
    ];

    casks = [
      "google-chrome" # Google Meet.
      "slack"         # The usual rabbit-hole of channels.
      "1password-cli" # Team's 1password
      "postman"       # Because it is more practical than curl
    ];
  };

  home-manager.users."${username}" = {
    imports = [ ../../home/config ];

    # Consider moving some of these packages to project's shell.nix if team's okay with that.
    home.packages = with pkgs; [
      # Cloud Providers
      google-cloud-sdk
      awscli2
      granted # Follow https://docs.commonfate.io/granted/getting-started/ to set it up

      # Kubernetes
      kubectl
      kubelogin-oidc

      # Infra
      terraform
    ];

    # Extra Firefox profiles
    programs.firefox.profiles.default.containers = {
      "OVO Energy" = {
        id = 8;
        color = "green";
        icon = "briefcase";
      };
      "Kaluza" = {
        id = 9;
        color = "blue";
        icon = "briefcase";
      };
    };

    home.stateVersion = "22.11";
  };

  system.stateVersion = 4;
}
