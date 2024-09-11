{ config, pkgs, lib, ... }:
{
  imports = [
    ../../config/home-manager
    ../../config/home-manager/coding/scala
    ../../config/home-manager/writing/logseq
  ];

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
