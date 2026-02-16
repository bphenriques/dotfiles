# Homelab Architecture

This document explains the homelab infrastructure managed via NixOS.

## Overview

The homelab runs on a NixOS host (`compute`) with services exposed via Traefik reverse proxy.
Services use Pocket-ID for OIDC authentication and are organized under `custom.home-server`.

## Module Structure

```
modules/nixos/home-server/
├── services.nix      # Service metadata, Traefik routes, dashboard generation
├── oidc.nix          # OIDC client management and credential propagation
├── users.nix         # User definitions per service
├── media.nix         # Media quality profiles (recyclarr/TRaSH guides)
└── virtualization.nix
```

## Service Pattern

Each service registers itself with `custom.home-server.services.<name>`:

```nix
custom.home-server.services.myapp = {
  port = 8080;
  forwardAuth = {
    enable = true;
    group = "admin";  # Required when forwardAuth is enabled
  };
  dashboard = {
    enable = true;
    category = "Media";
    description = "My Application";
    icon = "myapp.svg";
  };
};
```

This automatically:
- Creates a Traefik route at `https://myapp.<domain>`
- Adds forward auth middleware (if enabled)
- Adds an entry to the homepage dashboard

## Post-Start Configuration

Services that need runtime configuration use a oneshot systemd service with nushell scripts:

```
hosts/compute/services/<name>/
├── default.nix           # Service definition
├── post-start.nix        # Configure service (optional)
└── <name>-configure.nu   # Nushell script for API calls
```

Pattern:
- `systemd.services.<name>-configure` runs after the main service
- Uses `partOf` to restart when the main service restarts
- Uses `restartTriggers` to re-run when Nix config changes
- Scripts must be **idempotent** (safe to run multiple times)

## Secret Management

Secrets are managed via sops-nix:

```nix
# Declare secrets
sops.secrets."myapp/api-key" = { };

# Use in environment file template
sops.templates."myapp.env".content = ''
  API_KEY=${config.sops.placeholder."myapp/api-key"}
'';

# Or mount directly into containers
volumes = [
  "${config.sops.secrets."myapp/api-key".path}:/run/secrets/api_key:ro"
];
```

## OIDC Integration

Services using OIDC register as clients:

```nix
custom.home-server.oidc.clients.myapp.callbackURLs = [ 
  "${serviceCfg.publicUrl}/oauth2/callback" 
];
```

This:
1. Creates `/var/lib/homelab-oidc/myapp/` with `id` and `secret` files
2. Creates a group `homelab-oidc-myapp` for credential access
3. Registers the client with Pocket-ID via `pocket-id-configure.service`

Consumer pattern:
```nix
systemd.services.podman-myapp = {
  after = [ oidcCfg.systemd.provisionedTarget ];
  wants = [ oidcCfg.systemd.provisionedTarget ];
  serviceConfig.SupplementaryGroups = [ oidcClient.group ];
};
```

## Factory Functions

### mkArrService

Creates a standardized *arr service (radarr/sonarr):

```nix
import ./lib/mkArrService.nix { inherit config pkgs lib self; } {
  name = "radarr";
  port = 9098;
  description = "Movie Tracker";
  rootPath = config.custom.paths.media.movies;
  categoryField = "movieCategory";
  forwardAuthGroup = config.custom.home-server.groups.admin;
  downloadClient = "transmission";
}
```

This handles:
- Service registration with Traefik + dashboard
- sops secrets for API key
- NixOS service configuration
- Post-start configuration via `arr-configure.nu`

## Adding a New Service

1. Create `hosts/compute/services/<name>/default.nix`
2. Register `custom.home-server.services.<name>` with port and dashboard
3. Add container via `virtualisation.oci-containers.containers.<name>`
4. Add secrets via `sops.secrets`
5. (Optional) Add OIDC client via `custom.home-server.oidc.clients.<name>`
6. (Optional) Add post-start configuration with nushell script
7. Import in `hosts/compute/services/default.nix`

## Post-Configuration Scripts

| Service | Script | Purpose |
|---------|--------|---------|
| Pocket-ID | `pocket-id-configure.nu` | Provision users, groups, OIDC clients |
| RomM | `romm-configure.nu` | Create admin user |
| *arr services | `arr-configure.nu` | Configure root folders, download client |
| Immich | `immich-configure.nu` | Admin setup, users, external libraries |
| Miniflux | `miniflux-configure.nu` | User management |
