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
    terraform
  ];

  # xdg.userDirs not supported in MacOS
  home.sessionVariables = {
    XDG_DOCUMENTS_DIR = "${config.home.homeDirectory}/workspace";
  };

  home.stateVersion = "22.11";
}
