> **Auto-generated** from the NixOS service registry (`nix run .#service-catalogue -- compute`). Do not edit manually.

### General

| Name | Description | Version | Subdomain | Internal Port | Auth | Backup |
|------|-------------|---------|-----------|---------------|------|--------|
| [Actual Budget](https://actualbudget.org/) | Budget Manager | 26.2.1 | `budget` | 5006 | OIDC | ✓ |
| [File Browser](https://filebrowser.org) | File Browser | 2.57.1 | `files` | 8085 | ForwardAuth | — |
| [Home Assistant](https://home-assistant.io/) | Home Automation | 2026.2.3 | `home` | 8123 | ForwardAuth | — |
| [Miniflux](https://miniflux.app/) | RSS Server | 2.2.17 | `miniflux` | 8081 | OIDC | ✓ |
| [OpenCloud](https://github.com/opencloud-eu/opencloud) | Cloud Storage & Office | 5.1.0 | `cloud` | 9200 | OIDC | ✓ |
| [Radicale](https://radicale.org/v3.html) | CalDAV & CardDAV | 3.6.1 | `radicale` | 5232 | ForwardAuth | ✓ |
| [Syncthing](https://syncthing.net/) | File Sync | 2.0.12 | `syncthing` | 8384 | ForwardAuth | — |
| [Tandoor](https://tandoor.dev/) | Recipe Manager | 2.3.6 | `tandoor` | 9092 | OIDC | ✓ |

### Media

| Name | Description | Version | Subdomain | Internal Port | Auth | Backup |
|------|-------------|---------|-----------|---------------|------|--------|
| [Cleanuparr](https://github.com/cleanuparr/cleanuparr) | Queue Cleanup | 2.4.7 | `cleanuparr` | 11011 | ForwardAuth | — |
| [coturn](https://github.com/coturn/coturn) | STUN/TURN Server | 4.9.0 | `coturn` | 3478 | — | — |
| [Immich](https://immich.app/) | Photo & Video Gallery | 2.5.6 | `photos` | 2283 | OIDC | — |
| [Jellyfin](https://jellyfin.org/) | Media Player | 10.11.6 | `jellyfin` | 8096 | OIDC | — |
| [Jellyseerr](https://github.com/Fallenbagel/jellyseerr) | TV / Movie Finder | 2.7.3 | `jellyseerr` | 9099 | — | — |
| [Kavita](https://kavitareader.com) | Book Server | 0.8.8.3 | `kavita` | 8097 | OIDC | — |
| [Prowlarr](https://prowlarr.com/) | Manage *rr services | 2.3.0.5236 | `prowlarr` | 9096 | ForwardAuth | — |
| [Radarr](https://radarr.video) | Movie Tracker | 6.0.4.10291 | `radarr` | 9098 | ForwardAuth | ✓ |
| [RomM](https://github.com/rommapp/romm) | ROM Manager | 4.7.0 | `romm` | 8095 | OIDC | — |
| [Sonarr](https://sonarr.tv) | TV Tracker | 4.0.16.2944 | `sonarr` | 9097 | ForwardAuth | ✓ |
| [Transmission](https://www.transmissionbt.com/) | Torrent Client | 4.1.1 | `transmission` | 9091 | ForwardAuth | — |

### Monitoring

| Name | Description | Version | Subdomain | Internal Port | Auth | Backup |
|------|-------------|---------|-----------|---------------|------|--------|
| [Alertmanager](https://prometheus.io/docs/alerting/latest/alertmanager/) | Alert Routing | 0.31.1 | `alertmanager` | 9093 | ForwardAuth | — |
| [Grafana](https://grafana.com) | Dashboards | 12.4.0 | `grafana` | 3010 | ForwardAuth | — |
| [Ntfy](https://ntfy.sh) | Push Notifications | 2.17.0 | `ntfy` | 2586 | — | — |
| [Prometheus](https://prometheus.io) | Metrics | 3.10.0 | `prometheus` | 9090 | ForwardAuth | — |

### Administration

| Name | Description | Version | Subdomain | Internal Port | Auth | Backup |
|------|-------------|---------|-----------|---------------|------|--------|
| [Pocket ID](https://pocket-id.org) | OIDC Provider | 2.3.0 | `auth` | 8094 | — | — |
| [wireguard](https://www.wireguard.com/) | VPN | N/A | `wireguard` | 51820 | — | — |

### Infrastructure

| Name | Description | Version | Subdomain | Internal Port | Auth | Backup |
|------|-------------|---------|-----------|---------------|------|--------|
| [CouchDB](https://couchdb.apache.org) | Document Database | 3.5.1 | `couchdb` | 5984 | — | — |
| [homepage](https://gethomepage.dev) | Dashboard | 1.10.1 | `homepage` | 3001 | — | — |
| [tinyauth](https://tinyauth.app) | ForwardAuth Gateway | 5.0.1 | `tinyauth` | 3000 | OIDC | — |

