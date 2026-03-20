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
- **Backblaze B2**: Off-site encrypted backups via [rustic](https://github.com/rustic-rs/rustic) (daily backup, weekly verification). Recovery _should_ be documented.
- **Prometheus + Alertmanager**: Metrics, HTTP probes, and alerting.
- **[ntfy](https://ntfy.sh)** — Push notifications for system alerts and service events

All services bind to `127.0.0.1`; only Traefik (80/443) and Wireguard are exposed on the firewall.

Full service list in [`services.md`](./services.md) (auto-generated via `nix run .#service-catalogue`).

## Design

This design fits my small-scale homelab and is not meant to be an enterprise environment, therefore I simplify where
reasonable. Above all, I love reproducibility (hence `NixOS`), and low-maintenance ops as this is not another job.

- **Service registry**: [`custom.homelab.services.*`](../../modules/nixos/homelab/services-registry.nix): routing, auth, secrets, and integrations from a single declaration
- **[Layered access control](#access-control)**: Three tiers (`admin`, `users`, `guests`) enforced via OIDC per-client group restriction or ForwardAuth, never both on the same service
- **Secret provisioning** with per-service group isolation and systemd ordering:
  - **OIDC Clients** [provisioned from declarations](../../modules/nixos/homelab/security/oidc.nix)
  - **Runtime Secrets** such as API keys [generated at boot](../../modules/nixos/homelab/security/secrets.nix)
- **Reasonable hardening**: leans on NixOS and systemd defaults for service isolation
- **[User provisioning](../../modules/nixos/homelab/users.nix)**: central module to configure what each user has access to. Guest users managed via `pocket-id-manage` CLI

## Miniflux Example

### Service Registry

[`services/miniflux/default.nix`](./services/miniflux/default.nix) wires routing, auth, homepage, and metadata:

```nix
custom.homelab.services.miniflux = {
  metadata.description = "RSS Server";
  metadata.version = config.services.miniflux.package.version;
  metadata.homepage = config.services.miniflux.package.meta.homepage;
  metadata.category = "General";
  port = 8081;
  healthcheck.path = "/healthcheck";
  access.allowedGroups = with config.custom.homelab.groups; [ admin ];
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

Backup hooks are systemd oneshot services (`homelab-backup-<name>`) started by the main `homelab-backup` unit via `Wants=` + `After=`. Hook failures do not block the backup but trigger ntfy failure notifications via the task integration. The `OUTPUT_DIR` environment variable is set automatically by the backup module, and the directory is created fresh before each hook run and cleaned up after a successful backup:
```nix
custom.homelab.services.miniflux.backup.package = pkgs.writeShellApplication {
  name = "backup-miniflux";
  runtimeInputs = [ pkgs.curl ];
  text = ''
    export MINIFLUX_URL="${serviceCfg.url}"
    export MINIFLUX_ADMIN_PASSWORD_FILE="${serviceCfg.secrets.files.admin-password.path}"
    source ${./backup.sh}
  '';
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

## Access Control

Access control is enforced through OIDC [per-client group restriction](../../modules/nixos/homelab/security/oidc.nix) at the service level or through `ForwardAuth` middleware.

| Tier | Group | Intended for | Example access |
|------|-------|-------------|----------------|
| Admin | `admin` | Homelab owner | Everything |
| Users | `users` | Family | Media, recipes |
| Guests | `guests` | Friends, colleagues | Romm only (viewer) |

Guests are managed imperatively via `pocket-id-manage` to keep things simpler:

```bash
pocket-id-manage guest invite someone@work.com --firstName John --lastName Doe
wg-manage add john colleague@work.com
```
