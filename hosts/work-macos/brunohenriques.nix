{ config, pkgs, lib, ... }:
{
  imports = [
    ../../home-manager
    ../../home-manager/desktop
  ];

  # Consider moving some of these packages to project's shell.nix if team's okay with that.
  home.packages = [
    pkgs.awscli2
    pkgs.kubectl
    pkgs.kubelogin-oidc
    pkgs.tfswitch
  ];

  custom.programs.project.directory = "${config.home.homeDirectory}/workspace";

  home.stateVersion = "22.11";
}
