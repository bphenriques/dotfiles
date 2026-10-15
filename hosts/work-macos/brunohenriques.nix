{ pkgs, ... }:
{
  imports = [
    ../../profiles/home-manager/base.nix
    ../../profiles/home-manager/desktop
    ../../profiles/home-manager/development
  ];

  home.packages = [
    pkgs.awscli2
    pkgs.kubectl
    pkgs.go-task
    pkgs.kubectx
  ];

  programs.fish.plugins = [
    { name = "nvm"; inherit (pkgs.fishPlugins.nvm) src; }
  ];

  my.dotfiles.enable = true;

  home.stateVersion = "26.05";
}
