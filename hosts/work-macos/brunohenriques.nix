{ config, pkgs, lib, ... }:
{
  imports = [ ../../config/home-manager ];

  xdg.userDirs = {
    enable = true;
    createDirectories = false;

    documents = "${config.home.homeDirectory}/workspace";
    download  = "${config.home.homeDirectory}/Downloads";
  };

  # Consider moving some of these packages to project's shell.nix if team's okay with that.
  home.packages = with pkgs; [
    awscli2

    # Kubernetes
    kubectl
    kubelogin-oidc

    # Infra
    terraform
  ];

  home.stateVersion = "22.11";
}
