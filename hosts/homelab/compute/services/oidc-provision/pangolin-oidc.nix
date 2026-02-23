# OIDC Client for Pangolin (runs on gateway VM)
#
# This creates the Pocket-ID OIDC client for Pangolin.
# Credentials are provisioned to /run/homelab-oidc/pangolin/
# by the homelab-oidc-provision service.
#
# After deployment:
# 1. Get credentials: cat /run/homelab-oidc/pangolin/{id,secret}
# 2. Add Pocket-ID as IdP in Pangolin UI with these credentials

{ self, ... }:
let
  baseDomain = self.settings.compute.domain;
in
{
  custom.home-server.oidc.clients.pangolin.callbackURLs = [
    "https://pangolin.${baseDomain}/api/v1/auth/oidc/callback"
  ];
}
