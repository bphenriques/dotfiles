{ config, pkgs, lib, self, ... }:

let
  username = "brunohenriques";
  wallpapers = self.packages.wallpapers.override {
    selected = [ "lake-fishing-sunset" "mountains" "watch-tower" ];
  };
in
{
  imports = [ ../../profiles/darwin ];

  nix.settings.trusted-users = [ username ];
  users.users.${username}.home  = "/Users/${username}";
  home-manager.users.${username} = ./brunohenriques.nix;

  system.defaults.screencapture.location = "/Users/${username}/Pictures/screenshots";  # Avoid bloating the Desktop with screenshots.
  system.desktop.picture = wallpapers.files.mountains;

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
