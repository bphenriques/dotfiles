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
  secrets.files.api-key = { rotatable = true; };  # Runtime secret (generated at boot)
  secrets.systemd.dependentServices = [ "myapp" ];
  oidc.enable = true;
  oidc.systemd.dependentServices = [ "myapp" ];
  integrations.homepage = { enable = true; category = "Media"; description = "My App"; };
};
```

**Secrets**: Use `secrets.files` for internal secrets (generated at boot, rotatable). Use SOPS for external/restorable
secrets (encrypted in git).

See [services-registry.nix](../../modules/nixos/homelab/services-registry.nix), [secrets.nix](../../modules/nixos/homelab/secrets.nix), [oidc.nix](../../modules/nixos/homelab/oidc.nix).

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
