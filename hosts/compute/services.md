> **Auto-generated** from the NixOS service registry (`nix run .#service-catalogue -- compute`). Do not edit manually.

### General

| Name | Description | Version | Subdomain | Internal Port | Auth | Backup |
|------|-------------|---------|-----------|---------------|------|--------|
| [Bentopdf](https://bentopdf.com) | PDF Generator | 1.11.2 | `bentopdf` | 8092 | ForwardAuth | — |
| [Miniflux](https://miniflux.app/) | RSS Server | 2.2.18 | `miniflux` | 8081 | OIDC | ✓ |
| [My MPD](https://jcorporation.github.io/myMPD) | Remote MPD Client | 23.0.1 | `mympd` | 8093 | ForwardAuth | — |
| [Radicale](https://radicale.org/v3.html) | CalDAV & CardDAV | 3.6.1 | `radicale` | 5232 | ForwardAuth | ✓ |

### Media

| Name | Description | Version | Subdomain | Internal Port | Auth | Backup |
|------|-------------|---------|-----------|---------------|------|--------|
| [coturn](https://github.com/coturn/coturn) | STUN/TURN Server | 4.9.0 | `coturn` | 3478 | — | — |
| [Immich](https://immich.app/) | Photo & Video Gallery | 2.6.3 | `photos` | 2283 | OIDC | — |
| [Jellyfin](https://jellyfin.org/) | Media Player | 10.11.7 | `jellyfin` | 8096 | OIDC | — |
| [Kavita](https://kavitareader.com) | Book Server | 0.8.9.1 | `kavita` | 8097 | OIDC | — |
| [Prowlarr](https://prowlarr.com/) | Manage *rr services | 2.3.0.5236 | `prowlarr` | 9096 | ForwardAuth | — |
| [Radarr](https://radarr.video) | Movie Tracker | 6.0.4.10291 | `radarr` | 9098 | ForwardAuth | ✓ |
| [RomM](https://github.com/rommapp/romm) | ROM Manager | 4.8.1 | `romm` | 8095 | OIDC | — |
| [Seerr](https://github.com/seerr-team/seerr) | TV / Movie Finder | 3.1.0 | `seerr` | 9099 | — | — |
| [Sonarr](https://sonarr.tv) | TV Tracker | 4.0.17.2952 | `sonarr` | 9097 | ForwardAuth | ✓ |
| [Transmission](https://www.transmissionbt.com/) | Torrent Client | 4.1.1 | `transmission` | 9091 | ForwardAuth | — |

### Monitoring

| Name | Description | Version | Subdomain | Internal Port | Auth | Backup |
|------|-------------|---------|-----------|---------------|------|--------|
| [Alertmanager](https://prometheus.io/docs/alerting/latest/alertmanager/) | Alert Routing | 0.31.1 | `alertmanager` | 9093 | ForwardAuth | — |
| [Grafana](https://grafana.com) | Dashboards | 12.4.2 | `grafana` | 3010 | ForwardAuth | — |
| [Ntfy](https://ntfy.sh) | Push Notifications | 2.20.1 | `ntfy` | 2586 | — | — |
| [Prometheus](https://prometheus.io) | Metrics | 3.10.0 | `prometheus` | 9090 | ForwardAuth | — |

### Administration

| Name | Description | Version | Subdomain | Internal Port | Auth | Backup |
|------|-------------|---------|-----------|---------------|------|--------|
| [Pocket ID](https://pocket-id.org) | OIDC Provider | 2.4.0 | `auth` | 8094 | — | — |
| [wireguard](https://www.wireguard.com/) | VPN | N/A | `wireguard` | 51820 | — | — |

### Infrastructure

| Name | Description | Version | Subdomain | Internal Port | Auth | Backup |
|------|-------------|---------|-----------|---------------|------|--------|
| [homepage](https://gethomepage.dev) | Dashboard | 1.11.0 | `homepage` | 3001 | — | — |
| [tinyauth](https://tinyauth.app) | ForwardAuth Gateway | 5.0.4 | `tinyauth` | 3000 | OIDC | — |

### Home

| Name | Description | Version | Subdomain | Internal Port | Auth | Backup |
|------|-------------|---------|-----------|---------------|------|--------|
| [File Browser](https://filebrowser.org) | File Browser | 2.61.2 | `files` | 8085 | ForwardAuth | — |
| [Home Assistant](https://home-assistant.io/) | Home Automation | 2026.3.4 | `home` | 8123 | — | ✓ |
| [LaraPaper](https://github.com/usetrmnl/larapaper) | E-Ink Display Server | 0.31.4 | `trmnl` | 4567 | ForwardAuth | — |
| [Syncthing](https://syncthing.net/) | File Sync | 2.0.15 | `syncthing` | 8384 | ForwardAuth | — |
| [Tandoor](https://tandoor.dev/) | Recipe Manager | 2.6.0 | `tandoor` | 9092 | OIDC | ✓ |

### Productivity

| Name | Description | Version | Subdomain | Internal Port | Auth | Backup |
|------|-------------|---------|-----------|---------------|------|--------|
| [Gitea](https://about.gitea.com) | Git Server | 1.25.5 | `git` | 3100 | OIDC | ✓ |
| [Grist](https://github.com/gristlabs/grist-core) | Spreadsheet Database | 1.7.12 | `grist` | 8484 | ForwardAuth | ✓ |
| [Voice Assistant](https://github.com/ggml-org/whisper.cpp) | Voice-to-text | 1.8.3 | `voice` | 8179 | ForwardAuth | — |

