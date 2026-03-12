# Homelab

Self-hosted infrastructure running on low-power hardware.

## Hosts

| Host                 | Hardware                      | Role         |
|----------------------|-------------------------------|--------------|
| [compute](./compute) | Beelink EQ14 (N150), 32GB RAM | NixOS server |
| [storage](./storage) | Synology DS923+               | NAS          |

## Architecture

```
                        ┌───────────────────────────────────────┐
                        │           Compute Server              │
                        │                                       │
Internet ──▶ Cloudflare ├──▶ Traefik ──┬──▶ Pocket-ID (OIDC)   │
                        │      ▲       ├──▶ Jellyfin, Immich   │
            Wireguard ──┼──────┘       ├──▶ Homepage           │
            (VPN)       │              │                       │
                        │              │   Storage (SMB) ◀─────┼───┐
                        └──────────────┼───────────────────────┘   │
                                       │                           │
                                       └───────▶ NAS (external) ◀──┘
```

- **Cloudflare**: DNS + HTTPS certificates via ACME DNS challenge
- **Wireguard**: VPN for remote access
- **Traefik**: Reverse proxy with TLS termination (routes to local and external hosts)
- **Pocket-ID**: OIDC provider for SSO
- **Tinyauth**: ForwardAuth middleware for services without native OIDC
- **SMB**: Services access NAS storage via SMB mounts

## Adding a Service

Register in `custom.homelab.services.<name>`:

```nix
custom.homelab.services.myapp = {
  port = 8080;
  secrets.files.api-key = { rotatable = true; };
  secrets.systemd.dependentServices = [ "myapp" ];
  oidc.enable = true;
  integrations.homepage = { enable = true; category = "Media"; description = "My App"; };
};
```

See [services-registry.nix](../../modules/nixos/homelab/services-registry.nix), [oidc.nix](../../modules/nixos/homelab/oidc.nix).

## Secrets

Generated at boot via `openssl rand`, stored in `/var/lib/homelab-secrets/<owner>/`. Use SOPS for external/restorable secrets (encrypted in git).

**Templates** render config files with placeholder substitution (via `replace-secret`). Templates can reference:
- Own secrets: `serviceCfg.secrets.placeholder.<name>`
- Other services' secrets: `otherCfg.secrets.placeholder.<name>` (cross-service)
- OIDC credentials: `serviceCfg.oidc.id.placeholder` / `serviceCfg.oidc.secret.placeholder`

Cross-owner and OIDC dependencies are **auto-detected** from placeholder patterns in template content.

```nix
# Service with own secrets + OIDC in one template
custom.homelab.services.myapp.secrets = {
  files.jwt-secret = { rotatable = true; };
  templates.env.content = ''
    JWT=${serviceCfg.secrets.placeholder.jwt-secret}
    OIDC_CLIENT_ID=${serviceCfg.oidc.id.placeholder}
  '';
  systemd.dependentServices = [ "myapp" ];
};

# Non-service (timer/task) using cross-service secrets
custom.homelab.secrets.mytask = {
  templates."config.yml".content = ''
    api_key: ${otherCfg.secrets.placeholder.api-key}
  '';
  systemd.dependentServices = [ "mytask" ];
};
```

**Rotation**: `sudo rm /var/lib/homelab-secrets/<owner>/<file> && sudo systemctl restart homelab-secrets-<owner>`

See [_secrets-schema.nix](../../modules/nixos/homelab/_secrets-schema.nix), [secrets.nix](../../modules/nixos/homelab/secrets.nix).

## Adding a User

Add to `custom.homelab.users.<name>`:

```nix
custom.homelab.users.alice = {
  email = "alice@example.com";
  firstName = "Alice";
  lastName = "Smith";
  services.immich.enable = true;
  services.jellyfin.enable = true;
  services.wireguard = { enable = true; devices = [{ name = "phone"; fullAccess = false; }]; };
};
```

See [users.nix](../../modules/nixos/homelab/users.nix).
