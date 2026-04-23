> **Auto-generated** from the NixOS service registry (`nix run .#service-catalogue -- compute`). Do not edit manually.

### General

| Name                                           | Description       | Version | Subdomain  | Internal Port | Auth        | Backup |
| ---------------------------------------------- | ----------------- | ------- | ---------- | ------------- | ----------- | ------ |
| [Bentopdf](https://bentopdf.com)               | PDF Generator     | 1.11.2  | `bentopdf` | 8092          | ForwardAuth | —      |
| [Miniflux](https://miniflux.app/)              | RSS Server        | 2.2.19  | `miniflux` | 8081          | OIDC        | ✓      |
| [My MPD](https://jcorporation.github.io/myMPD) | Remote MPD Client | 25.0.1  | `mympd`    | 8093          | —           | —      |
| [Radicale](https://radicale.org/v3.html)       | CalDAV & CardDAV  | 3.7.1   | `radicale` | 5232          | ForwardAuth | ✓      |

### Media

| Name                                            | Description           | Version     | Subdomain      | Internal Port | Auth        | Backup |
| ----------------------------------------------- | --------------------- | ----------- | -------------- | ------------- | ----------- | ------ |
| [coturn](https://github.com/coturn/coturn)      | STUN/TURN Server      | 4.9.0       | `coturn`       | 3478          | —           | —      |
| [Immich](https://immich.app/)                   | Photo & Video Gallery | 2.7.5       | `photos`       | 2283          | OIDC        | —      |
| [Jellyfin](https://jellyfin.org/)               | Media Player          | 10.11.8     | `jellyfin`     | 8096          | OIDC        | —      |
| [Kapowarr](https://github.com/Casvt/Kapowarr)   | Comic Book Manager    | 1.3.1       | `kapowarr`     | 5656          | ForwardAuth | —      |
| [Kavita](https://kavitareader.com)              | Book Server           | 0.8.9.1     | `kavita`       | 8097          | OIDC        | —      |
| [Prowlarr](https://prowlarr.com/)               | Manage \*rr services  | 2.3.5.5327  | `prowlarr`     | 9096          | ForwardAuth | —      |
| [Radarr](https://radarr.video)                  | Movie Tracker         | 6.1.1.10360 | `radarr`       | 9098          | ForwardAuth | ✓      |
| [RomM](https://github.com/rommapp/romm)         | ROM Manager           | 4.8.1       | `romm`         | 8095          | OIDC        | —      |
| [Seerr](https://github.com/seerr-team/seerr)    | TV / Movie Finder     | 3.1.0       | `seerr`        | 9099          | —           | —      |
| [Sonarr](https://sonarr.tv)                     | TV Tracker            | 4.0.17.2952 | `sonarr`       | 9097          | ForwardAuth | ✓      |
| [Transmission](https://www.transmissionbt.com/) | Torrent Client        | 4.1.1       | `transmission` | 9091          | ForwardAuth | —      |

### Monitoring

| Name                                                                     | Description        | Version | Subdomain      | Internal Port | Auth        | Backup |
| ------------------------------------------------------------------------ | ------------------ | ------- | -------------- | ------------- | ----------- | ------ |
| [Alertmanager](https://prometheus.io/docs/alerting/latest/alertmanager/) | Alert Routing      | 0.31.1  | `alertmanager` | 9093          | ForwardAuth | —      |
| [Grafana](https://grafana.com)                                           | Dashboards         | 12.4.2  | `grafana`      | 3010          | ForwardAuth | —      |
| [Ntfy](https://ntfy.sh)                                                  | Push Notifications | 2.21.0  | `ntfy`         | 2586          | —           | —      |
| [Prometheus](https://prometheus.io)                                      | Metrics            | 3.10.0  | `prometheus`   | 9090          | ForwardAuth | —      |

### Administration

| Name                                    | Description   | Version | Subdomain   | Internal Port | Auth | Backup |
| --------------------------------------- | ------------- | ------- | ----------- | ------------- | ---- | ------ |
| [Pocket ID](https://pocket-id.org)      | OIDC Provider | 2.5.0   | `auth`      | 8094          | —    | —      |
| [wireguard](https://www.wireguard.com/) | VPN           | N/A     | `wireguard` | 51820         | —    | —      |

### Infrastructure

| Name                                | Description         | Version | Subdomain  | Internal Port | Auth | Backup |
| ----------------------------------- | ------------------- | ------- | ---------- | ------------- | ---- | ------ |
| [homepage](https://gethomepage.dev) | Dashboard           | 1.12.1  | `homepage` | 3001          | —    | —      |
| [tinyauth](https://tinyauth.app)    | ForwardAuth Gateway | 5.0.6   | `tinyauth` | 3000          | OIDC | —      |

### Home

| Name                                         | Description     | Version  | Subdomain   | Internal Port | Auth        | Backup |
| -------------------------------------------- | --------------- | -------- | ----------- | ------------- | ----------- | ------ |
| [File Browser](https://filebrowser.org)      | File Browser    | 2.63.2   | `files`     | 8085          | ForwardAuth | —      |
| [Home Assistant](https://home-assistant.io/) | Home Automation | 2026.4.2 | `home`      | 8123          | —           | ✓      |
| [Syncthing](https://syncthing.net/)          | File Sync       | 2.0.15   | `syncthing` | 8384          | ForwardAuth | —      |
| [Tandoor](https://tandoor.dev/)              | Recipe Manager  | 2.6.6    | `tandoor`   | 9092          | OIDC        | ✓      |

### Productivity

| Name                                             | Description          | Version | Subdomain | Internal Port | Auth        | Backup |
| ------------------------------------------------ | -------------------- | ------- | --------- | ------------- | ----------- | ------ |
| [Gitea](https://about.gitea.com)                 | Git Server           | 1.25.5  | `git`     | 3100          | OIDC        | ✓      |
| [Grist](https://github.com/gristlabs/grist-core) | Spreadsheet Database | 1.7.12  | `grist`   | 8484          | ForwardAuth | ✓      |
| [Papra](https://github.com/papra-hq/papra)       | Document Management  | 26.4.0  | `papra`   | 1221          | OIDC        | —      |
