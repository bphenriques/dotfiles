{ config, pkgs, lib, ... }:

# TODO: Explore https://github.com/jarun/nnn
# TODO: Explore SyncThing: https://github.com/kclejeune/system/blob/master/modules/darwin/syncthing.nix#L40
let username = "brunohenriques";
in
{
  imports = [../../darwin];
  users.users."${username}".home  = "/Users/${username}";
  system.defaults.screencapture.location = "/Users/${username}/Pictures/screenshots";  # Avoid bloating the Desktop with screenshots.
  system.desktop.picture = ./wallpaper.png; # From simpledesktops

  homebrew = {
    taps = [
      "homebrew/cask"
      "common-fate/granted"
    ];

    brews = [
      "granted" # Follow https://docs.commonfate.io/granted/getting-started/ to set it up. FIXME: assumego missing if installing through nixpkgs
    ];

    casks = [
      "google-chrome" # Google Meet.
      "slack"         # The usual rabbit-hole of channels.
    ];
  };

  home-manager.users."${username}" = {
    imports = [
      ../../home
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

    home.stateVersion = "22.11";
  };

  system.stateVersion = 4;
}
