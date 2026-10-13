{ config, pkgs, lib, ... }:
let
  username = "bruno.henriques";
in
{
  imports = [
    ../../profiles/darwin
    #../../profiles/home-manager/development
  ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nixpkgs.hostPlatform = "aarch64-darwin";

  homebrew = {
    brews = [
      "go-task"
    ];
  };

  nix.settings.trusted-users = [ username ];
  system.primaryUser = username;
  users.users."${username}".home  = "/Users/${username}";
  home-manager.users."${username}" = ./brunohenriques.nix;

  system.stateVersion = 6;
}
