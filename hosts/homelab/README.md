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
- **Secrets**: Dual approach (see below)

## Guidelines

1. **Security first** - Passkeys, and minimal exposed ports.
2. **3-2-1 backups** - Local + external drive + Backblaze
3. **Declarative** - Everything defined in Nix

There is balance to be made as this is a homelab setup that is only accessible through Wireguard.

## Service Catalogue

Services are registered via `custom.homelab.services.<name>` with all concerns colocated:

```nix
custom.homelab.services.myapp = {
  host = "127.0.0.1";  # local service (default)
  port = 8080;
  
  # Per-service secrets (generated at boot)
  secrets = {
    files.api-key = { rotatable = true; };
    systemd.dependentServices = [ "myapp" ];
  };
  
  # Per-service OIDC client
  oidc = {
    enable = true;
    systemd.dependentServices = [ "myapp" ];
  };
  
  # Integrations with external tools
  integrations.homepage = {
    enable = true;
    category = "Media";
    description = "My App";
  };
};

# External service (on another host)
custom.homelab.services.synology = {
  host = "192.168.1.100";
  port = 5000;
  integrations.homepage = { enable = true; category = "Infrastructure"; description = "NAS"; };
};
```

OIDC credentials are accessed via:
1. **Direct file access** (preferred): `serviceCfg.oidc.idFile`/`secretFile` + `SupplementaryGroups`
2. **Systemd credentials**: `serviceCfg.oidc.systemd.loadCredentials` for scripts

Cross-service dependencies work by extending another service's config:
```nix
# jellyseerr needs radarr's secrets
custom.homelab.services.radarr.secrets.systemd.dependentServices = [ "jellyseerr-configure" ];
```

## Secrets

| Type | Use Case |
|------|----------|
| **SOPS** | External/restorable secrets (encrypted in git) |
| **Runtime** | Internal secrets with easy rotation (delete + restart). See [secrets module](../../modules/nixos/homelab/secrets.nix) |
