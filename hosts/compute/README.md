# Compute Server

Beelink EQ14 (N150+32GB RAM) running NixOS. Optimised for low maintenance, small attack surface, and declarative service management.

<p float="center">
  <img src="screenshots/homepage.png" width="49%" />
</p>

## Architecture

```
  Cloudflare           ┌────────────────────────────────────┐
  (DNS + ACME only)    │         Compute Server             │
                       │                                    │
  LAN / Wireguard ─────├──▶ Traefik ──┬──▶ Pocket-ID (OIDC) │
                       │              ├──▶ Jellyfin, Immich │
                       │              ├──▶ Homepage, ...    │
                       │              │                     │
                       └──────────────┼─────────────────────┘
                                      │
                                      ▼
                                ┌───────────┐
                                │    NAS    │
                                │ (Synology)│
                                └───────────┘
                                  SMB mounts
```

- **[Cloudflare](../infrastructure.md)**: DNS (records point to internal IP; not proxied) + ACME DNS-01 for certificate issuance (Traefik terminates TLS locally)
- **Traefik**: Reverse proxy; only externally reachable entry point alongside Wireguard
- **Pocket-ID**: OIDC provider with passkey authentication
- **Tinyauth**: ForwardAuth middleware for services without native OIDC
- **Wireguard**: VPN for remote access
- **SMB**: Services access [NAS](../storage) storage via SMB mounts
- **Backblaze B2**: Off-site encrypted backups via [rustic](https://github.com/rustic-rs/rustic) (daily backup, weekly verification)
- **Prometheus + Alertmanager**: Metrics, HTTP probes, and alerting.
- **[ntfy](https://ntfy.sh)** — Push notifications for system alerts and service events

All services bind to `127.0.0.1`; only Traefik (80/443) and Wireguard are exposed on the firewall.

Full service list in [`services.md`](./services.md) (auto-generated via `nix run .#service-catalogue`).

## Design

This design fits my small-scale homelab and is not meant to be an enterprise environment, therefore I simplify where
reasonable. Above all, I love reproducibility (hence `NixOS`), and low-maintenance ops as this is not another job.

- **Service registry**: [`custom.homelab.services.*`](../../modules/nixos/homelab/services-registry.nix): routing, auth, secrets, and integrations from a single declaration
- **Layered access control**: OIDC SSO where supported, [`ForwardAuth`](../../modules/nixos/homelab/ingress.nix) fallback where not
- **Secret provisioning** with per-service group isolation and systemd ordering:
  - **OIDC Clients** [provisioned from declarations](../../modules/nixos/homelab/security/oidc.nix)
  - **Runtime Secrets** such as API keys [generated at boot](../../modules/nixos/homelab/security/secrets.nix)
- **Reasonable hardening**: leans on NixOS and systemd defaults for service isolation
- **[User provisioning](../../modules/nixos/homelab/users.nix)**: central module to configure what each user has access to
- **[Central Backup timer](../../modules/nixos/homelab/backup.nix)**: supports pre-backup hooks for customizations

## Miniflux Example

### Service Registry

[`services/miniflux/default.nix`](./services/miniflux/default.nix) wires routing, auth, homepage, and metadata:

```nix
custom.homelab.services.miniflux = {
  description = "RSS Server";
  version = config.services.miniflux.package.version;
  homepage = config.services.miniflux.package.meta.homepage;
  category = "General";
  port = 8081;
  healthcheck.path = "/healthcheck";
  oidc = {
    enable = true;
    systemd.dependentServices = [ "miniflux" "miniflux-configure" ];
  };
  integrations.homepage.enable = true;
};
```

From this, the service gets a Traefik route, OIDC client, systemd ordering, a homepage entry, and a health check endpoint for monitoring and dashboard status.

### OIDC wiring

OIDC credentials are consumed via files to ensure we do not leak secrets to Nix store:
```nix
services.miniflux.config = {
  OAUTH2_CLIENT_ID_FILE     = serviceCfg.oidc.id.file;
  OAUTH2_CLIENT_SECRET_FILE = serviceCfg.oidc.secret.file;
  OAUTH2_OIDC_DISCOVERY_ENDPOINT = oidcCfg.provider.issuerUrl;
};
```

### Runtime secrets

Runtime secrets like the admin password are generated at boot:

```nix
custom.homelab.services.miniflux.secrets = {
  files.admin-password = { rotatable = false; };
  templates."admin-credentials.env".content = ''
    ADMIN_USERNAME=admin
    ADMIN_PASSWORD=${serviceCfg.secrets.placeholder.admin-password}
  '';
  systemd.dependentServices = [ "miniflux" "miniflux-configure" ];
};
```

Rotation is done calling:
```
sudo rm /var/lib/homelab-secrets/miniflux/admin-password && sudo systemctl restart homelab-secrets-miniflux
```

### Backup

Pre-backup hook that exports data before the nightly run:
```nix
custom.homelab.services.miniflux.backup = {
  script = ./backup.sh;
  environment = {
    MINIFLUX_URL = serviceCfg.url;
    MINIFLUX_ADMIN_PASSWORD_FILE = serviceCfg.secrets.files.admin-password.path;
    OUTPUT_DIR = "${backupCfg.extrasDir}/miniflux";
  };
};
```

### User access

Users are declared centrally; per-service flags drive OIDC provisioning and configure-time setup:

```nix
custom.homelab.users.alice = {
  email = "alice@example.com";
  firstName = "Alice";
  lastName = "Smith";
  services.immich.enable = true;
  services.jellyfin.enable = true;
  services.miniflux.enable = true;
};
```
