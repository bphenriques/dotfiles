{ config, pkgs, lib, ... }:
{
  imports = [ ../../config/home-manager ];

  # Consider moving some of these packages to project's shell.nix if team's okay with that.
  home.packages = with pkgs; [
    awscli2

    # Kubernetes
    kubectl
    kubelogin-oidc

    # Infra
    tfswitch
  ];

  custom.programs.project.directory = "${config.home.homeDirectory}/workspace";

  home.stateVersion = "22.11";
}

# TODO: https://github.com/isabelroses/dotfiles/tree/main/modules/darwin