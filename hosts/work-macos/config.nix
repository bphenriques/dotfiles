{ config, pkgs, lib, self, ... }:

let
  username = "brunohenriques";
  wallpapers = self.private.wallpapers.override {
    selected = [ "lake-fishing-sunset" "mountains" "whale-sunset" "watch-tower" ];
  };
in
{
  imports = [ ../../config/darwin ];

  nix.settings.trusted-users = [ username ];
  users.users."${username}".home  = "/Users/${username}";
  home-manager.users."${username}" = ./brunohenriques.nix;

  system.defaults.screencapture.location = "/Users/${username}/Pictures/screenshots";  # Avoid bloating the Desktop with screenshots.
  system.desktop.picture = "${wallpapers}/share/wallpapers/mountains.png";

  homebrew = {
    taps = [
      "coursier/formulas" # Scala
    ];
    brews = [
      "coursier"  # Scala
      "python3"   # Implicit dependency of Aiven client
      "kubeseal"  # K8s stuff
      "go-task"
    ];

    casks = [
      "bloop"         # Scala
      "google-chrome" # Google Meet.
      "slack"         # The usual rabbit-hole of channels.
      "1password-cli" # Team's 1password
      "postman"       # Because it is more practical than curl
    ];
  };

  system.stateVersion = 4;
}
