# Pangolin Reverse Proxy
#
# Edge reverse proxy with identity-aware access control.
# Uses Pocket-ID (auth VM) as the single OIDC provider.
#
# The NixOS pangolin module manages Traefik internally - no separate Traefik config needed.
#
# Post-deployment setup required (one-time):
# 1. Access https://pangolin.<domain>/auth/initial-setup
# 2. Create admin account
# 3. Create Root API Key in Server Admin → API Keys
# 4. Add API key to secrets.yaml for automation
#
# After initial setup, pangolin-provision service will:
# - Add Pocket-ID as OIDC provider
# - Create resources (auth, romm, etc.)
# - Configure access policies based on groups

{ config, pkgs, lib, self, ... }:
let
  networking = import ../../networking.nix;
  baseDomain = self.settings.compute.domain;
  authDomain = "auth.${baseDomain}";
  cloudflareEmail = self.settings.cloudflareEmail;
in
{
  sops.secrets = {
    "pangolin/server-secret" = { };
    "cloudflare/dns-api-token" = { };
  };

  sops.templates."pangolin.env" = {
    content = ''
      SERVER_SECRET=${config.sops.placeholder."pangolin/server-secret"}
    '';
  };

  sops.templates."traefik.env" = {
    owner = "traefik";
    content = ''
      CF_DNS_API_TOKEN=${config.sops.placeholder."cloudflare/dns-api-token"}
    '';
  };

  services.pangolin = {
    enable = true;
    baseDomain = baseDomain;
    dashboardDomain = "pangolin.${baseDomain}";
    letsEncryptEmail = cloudflareEmail;
    dnsProvider = "cloudflare";
    environmentFile = config.sops.templates."pangolin.env".path;

    settings = {
      app = {
        save_logs = true;
        log_level = "info";
      };

      domains.domain1 = {
        prefer_wildcard_cert = true;
      };

      flags = {
        # Enable REST API for automation
        enable_integration_api = true;

        # Security: invite-only, no self-signup
        require_email_verification = false;
        disable_signup_without_invite = true;
        disable_user_create_org = true;

        # Allow direct HTTP resources (for internal services)
        allow_raw_resources = true;
      };
    };
  };

  services.traefik.environmentFiles = [ config.sops.templates."traefik.env".path ];

  # Resolve auth domain to internal IP for OIDC API calls
  # This allows Pangolin to talk directly to Pocket-ID without going through Cloudflare
  networking.hosts.${networking.vm.auth.ip} = [ authDomain ];
}
