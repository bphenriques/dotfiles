> **Auto-generated** from the NixOS service registry (`nix run .#service-catalogue -- compute`). Do not edit manually.

### General

| Name | Description | Version | Subdomain | Internal Port | Auth | Scope |
|------|-------------|---------|-----------|---------------|------|-------|
| [Home Assistant](https://home-assistant.io/) | Home Automation | 2026.2.3 | `home` | 8123 | ForwardAuth | users |
| [miniflux](https://miniflux.app/) | RSS Server | 2.2.17 | `miniflux` | 8081 | OIDC | users |
| [OpenCloud](https://github.com/opencloud-eu/opencloud) | Cloud Storage & Office | 5.1.0 | `cloud` | 9200 | OIDC | users |
| [Radicale](https://radicale.org/v3.html) | CalDAV & CardDAV | 3.6.1 | `radicale` | 5232 | ForwardAuth | admin |
| [syncthing](https://syncthing.net/) | File Sync | 2.0.12 | `syncthing` | 8384 | ForwardAuth | admin |
| [Tandoor](https://tandoor.dev/) | Recipe Manager | 2.3.6 | `recipes` | 9092 | OIDC | users |

### Media

| Name | Description | Version | Subdomain | Internal Port | Auth | Scope |
|------|-------------|---------|-----------|---------------|------|-------|
| [cleanuparr](https://github.com/cleanuparr/cleanuparr) | Queue Cleanup | 2.4.7 | `cleanuparr` | 11011 | ForwardAuth | admin |
| [immich](https://immich.app/) | Photo & Video Gallery | 2.5.6 | `photos` | 2283 | OIDC | users |
| [jellyfin](https://jellyfin.org/) | Media Player | 10.11.6 | `jellyfin` | 8096 | OIDC | users |
| [jellyseerr](https://github.com/Fallenbagel/jellyseerr) | TV / Movie Finder | 2.7.3 | `jellyseerr` | 9099 | — | — |
| [kavita](https://kavitareader.com) | Book Server | 0.8.8.3 | `kavita` | 8097 | OIDC | users |
| [prowlarr](https://prowlarr.com/) | Manage *rr services | 2.3.0.5236 | `prowlarr` | 9096 | ForwardAuth | admin |
| [radarr](https://radarr.video) | Movie Tracker | 6.0.4.10291 | `radarr` | 9098 | ForwardAuth | admin |
| [romm](https://github.com/rommapp/romm) | ROM Manager | 4.7.0 | `romm` | 8095 | OIDC | users |
| [sonarr](https://sonarr.tv) | TV Tracker | 4.0.16.2944 | `sonarr` | 9097 | ForwardAuth | admin |
| [transmission](https://www.transmissionbt.com/) | Torrent Client | 4.1.1 | `transmission` | 9091 | ForwardAuth | admin |

### Monitoring

| Name | Description | Version | Subdomain | Internal Port | Auth | Scope |
|------|-------------|---------|-----------|---------------|------|-------|
| [alertmanager](https://prometheus.io/docs/alerting/latest/alertmanager/) | Alert Routing | 0.31.1 | `alertmanager` | 9093 | ForwardAuth | admin |
| [grafana](https://grafana.com) | Dashboards | 12.4.0 | `grafana` | 3010 | ForwardAuth | admin |
| [ntfy](https://ntfy.sh) | Push Notifications | 2.17.0 | `ntfy` | 2586 | — | — |
| [prometheus](https://prometheus.io) | Metrics | 3.10.0 | `prometheus` | 9090 | ForwardAuth | admin |

### Administration

| Name | Description | Version | Subdomain | Internal Port | Auth | Scope |
|------|-------------|---------|-----------|---------------|------|-------|
| [Pocket ID](https://pocket-id.org) | OIDC Provider | 2.3.0 | `auth` | 8094 | — | — |
| [wireguard](https://www.wireguard.com/) | VPN | 1.0 | `wireguard` | 51820 | — | — |

### Infrastructure

| Name | Description | Version | Subdomain | Internal Port | Auth | Scope |
|------|-------------|---------|-----------|---------------|------|-------|
| [CouchDB](https://couchdb.apache.org) | Document Database | 3.5.1 | `couchdb` | 5984 | — | — |
| [homepage](https://gethomepage.dev) | Dashboard | 1.10.1 | `homepage` | 3001 | — | — |
| [tinyauth](https://tinyauth.app) | ForwardAuth Gateway | 5.0.1 | `tinyauth` | 3000 | OIDC | users |

