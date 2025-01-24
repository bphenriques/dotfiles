{ config, pkgs, lib, ... }:
{
  imports = [
    ../../home-manager
    ../../home-manager/desktop
  ];

  # Consider moving some of these packages to project's shell.nix if team's okay with that.
  home.packages = with pkgs; [
    awscli2
    kubectl
    kubelogin-oidc
    tfswitch
  ];

  custom.programs.project.directory = "${config.home.homeDirectory}/workspace";

  home.stateVersion = "22.11";
}
