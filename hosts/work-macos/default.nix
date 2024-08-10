{ config, pkgs, lib, ... }:

let username = "brunohenriques";
in
{
  imports = [ ../../config/darwin.nix ];

  nix.settings.trusted-users = [ username ];
  users.users."${username}".home  = "/Users/${username}";
  home-manager.users."${username}" = ./brunohenriques.nix;

  # TODO: Make this parametrized per user
  system.defaults.screencapture.location = "/Users/${username}/Pictures/screenshots";  # Avoid bloating the Desktop with screenshots.
  system.desktop.picture = ./secrets/wallpaper.sops.png;

  homebrew = {
    taps = [
      "scalacenter/bloop" # Scala
      "coursier/formulas" # Scala
    ];
    brews = [
      "scalacenter/bloop/bloop"     # Scala
      "coursier/formulas/coursier"  # Scala
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

  system.stateVersion = 4;
}
