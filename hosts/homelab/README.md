# Homelab

Self-hosted infrastructure running on low-power hardware.

## Hosts

| Host | Hardware | Role |
|------|----------|------|
| [compute](./compute) | Beelink EQ14 (N150), 32GB RAM | NixOS server running services |
| [storage](./storage) | Synology DS923+ | NAS for media and backups |

## Architecture

```
┌─────────────┐     ┌─────────────┐
│   Clients   │────▶│  Wireguard  │
└─────────────┘     └──────┬──────┘
                           │
                    ┌──────▼──────┐
                    │   Traefik   │
                    │  (reverse   │
                    │   proxy)    │
                    └──────┬──────┘
                           │
              ┌────────────┼────────────┐
              │            │            │
       ┌──────▼──────┐ ┌───▼───┐ ┌──────▼──────┐
       │  Pocket-ID  │ │ Apps  │ │   Storage   │
       │   (OIDC)    │ │       │ │   (SAMBA)   │
       └─────────────┘ └───────┘ └─────────────┘
```

## Stack

- **Reverse Proxy**: Traefik with automatic HTTPS
- **Authentication**: Pocket-ID (OIDC provider). Tinyauth as forward auth when OIDC is not possible.
- **DNS**: Cloudflare
- **Remote Access**: Wireguard
- **Secrets**: SOPS/Age with per-host keys

## Guidelines

1. **Security first** - Passkeys, and minimal exposed ports.
2. **3-2-1 backups** - Local + external drive + Backblaze
3. **Declarative** - Everything defined in Nix

There is balance to be made as this is a homelab setup that is only accessible through Wireguard.

## Service Catalogue

Following a Service catalogue paradigm, services are registered via `custom.homelab.services.<name>`. Orthogonal concerns (ingress, dashboard, etc.) are derived from this registry, reducing boilerplate.

Each service registers their own OIDC client through `custom.homelab.oidc.clients.<name>` read using:
1. **Direct file access** (preferred): `idFile`/`secretFile` + `SupplementaryGroups`
2. **Systemd credentials**: `loadCredentials` + `%d/` paths for scripts

To reduce boilerplate, mounts and OIDC clients support `systemd.dependentServices` for auto-wiring dependencies:
```nix
custom.fileSystems.homelab.mounts.media.systemd.dependentServices = [ "jellyfin" ];
custom.homelab.oidc.clients.miniflux.systemd.dependentServices = [ "miniflux" ];
```

