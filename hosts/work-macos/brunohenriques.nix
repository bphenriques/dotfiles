{ config, pkgs, lib, ... }:
{
  imports = [
    ../../config/home-manager
    ../../config/home-manager/dev/scala
    ../../config/home-manager/media/logseq
  ];

  # Consider moving some of these packages to project's shell.nix if team's okay with that.
  home.packages = with pkgs; [
    # Cloud Providers
    (google-cloud-sdk.withExtraComponents [
      google-cloud-sdk.components.core
      google-cloud-sdk.components.bq
      google-cloud-sdk.components.gsutil
      google-cloud-sdk.components.gke-gcloud-auth-plugin
    ])
    awscli2
    granted # Follow https://docs.commonfate.io/granted/getting-started/ to set it up

    # Kubernetes
    kubectl
    kubelogin-oidc

    # Infra
    terraform
  ];

  home.stateVersion = "22.11";
}
