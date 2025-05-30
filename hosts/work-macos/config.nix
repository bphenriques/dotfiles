{ config, pkgs, lib, self, ... }:

let
  username = "brunohenriques";
  wallpapers = self.pkgs.wallpapers.override {
    selected = [ "lake-fishing-sunset" "mountains" "whale-sunset" "watch-tower" ];
  };
in
{
  imports = [ ../../config/darwin ];

  nix.settings.trusted-users = [ username ];
  users.users.${username}.home  = "/Users/${username}";
  home-manager.users.${username} = ./brunohenriques.nix;

  system.defaults.screencapture.location = "/Users/${username}/Pictures/screenshots";  # Avoid bloating the Desktop with screenshots.
  system.desktop.picture = "${wallpapers}/share/wallpapers/mountains.png";

  homebrew = {
    brews = [
      "go-task"
      "kubeseal"  # K8s stuff
      "snyk-cli"
    ];

    casks = [
      "google-chrome"
      "slack"
      "1password-cli"
      "postman"
    ];
  };

  system.stateVersion = 4;
}
