# Compute Server

NixOS selfhost server built using my **[`selfhost-nix`](https://github.com/bphenriques/selfhost-nix)** flake.

<p float="center">
  <img src="screenshots/homepage.png" width="49%" />
  <img src="screenshots/grafana.png" width="46%" />
</p>

## Hardware

- **Model**: Beelink EQ14
- **CPU**: Intel N150 (4 E-cores, shared CPU/iGPU die)
- **RAM**: 32GB

Made some tweaks to ensure thermal stability with sustained workloads:

- **BIOS fan curve** (`Del` → `Advanced → Hardware Monitor → Smart Fan Function`): adjust to run fans at full-speed at **80°C** (default is 90°C) and to start earlier with slope of 4 PWM/°C. Temperatures dropped from 83°C to ~65°C under identical load.
- **Systemd `throttled.slice`**: heavy services (Immich, Jellyfin) pinned to cores 1-2 (`AllowedCPUs`), hard-capped (`CPUQuota=150%`).
- **Systemd `critical.slice`**: SSH/DHCP.

## Architecture

```
  Cloudflare           ┌──────────────────────────────────────────┐
  (DNS + ACME only)    │            Compute Server                │
                       │                                          │
  LAN / Wireguard ─────├──▶ Traefik ──┬──▶ Pocket-ID (OIDC)       │
                       │              ├──▶ Tinyauth (ForwardAuth) │
                       │              ├──▶ Homepage               │
                       │              ├──▶ Jellyfin, Immich, ...  │
                       │              │                           │
                       │  Prometheus ─┼──▶ scrape targets         │
                       │       │      │                           │
                       │       ▼      │                           │
                       │  Alertmanager──▶ ntfy ──▶ push notif     │
                       │                                          │
                       │  rustic (cron) ──────────────────────────├──▶ Backblaze B2
                       └──────────────┬───────────────────────────┘    (off-site)
                                      │
                                      ▼
                                ┌───────────┐
                                │    NAS    │
                                │ (Synology)│
                                └───────────┘
                                  SMB mounts
```

## Access Control

| Group    | Target              | Example access     |
| -------- | ------------------- | ------------------ |
| `admin`  | Homelab owner       | Everything         |
| `users`  | Family              | Media, recipes     |
| `guests` | Friends, colleagues | Romm only (viewer) |

## Setup

| Dependency   | What                                                   | Reference                                                  |
| ------------ | ------------------------------------------------------ | ---------------------------------------------------------- |
| SMB Server   | Access to a SMB server                                 | [storage](../storage.md)                                   |
| Cloudflare   | DNS zone + API token (DNS-01 ACME challenge)           | [infrastructure](../infrastructure.md)                     |
| SMTP         | Account credentials for transactional email            |                                                            |
| Backblaze B2 | Bucket + application key for off-site backups          |                                                            |
| ZBT-2 Dongle | Nabu Casa Connect ZBT-2 (Thread/Matter radio)          | Flash OpenThread RCP firmware post-install                 |
| Secrets      | Bootstrap via `dotfiles-secrets init-host` (Bitwarden) | [`apps/nixos-install`](../../apps/nixos-install/README.md) |

Run [`apps/nixos-install`](../../apps/nixos-install/README.md): disko partitioning, secrets provisioning, NixOS install.

## Post-Install

Steps not worth automating: unstable APIs, wizard-driven, or tolerable one-time.

### Pocket-ID

Register admin passkey via browser (accept the invite received via email)

### Home Assistant

1. Complete onboarding wizard
2. Flash ZBT-2 with OpenThread RCP firmware using [Device Toolbox](https://toolbox.openhomefoundation.org/home-assistant-connect-zbt-2/) from a PC with Chrome
3. Add OTBR integration: Settings → Integrations → OTBR → `http://127.0.0.1:8091`
4. Add Thread integration: set OTBR network as preferred, enable Android/iOS credentials
5. Add Matter integration: accept default websocket URL
6. Sync Thread credentials in companion app, then commission Matter devices by scanning QR codes
7. Configure backup scheduler (UI): write backups to `/var/lib/hass/backups/` for off-site pickup

### Kapowarr

1. Set ComicVine API key: Settings → General → ComicVine API Key (get one at [comicvine.gamespot.com](https://comicvine.gamespot.com/api/))
2. Add root folder: Settings → Media Management → Root Folders → add `/comics`
3. Torrent client: only qBittorrent is supported (Transmission planned for V1.4.0); built-in direct download clients (GetComics, Mega, Pixeldrain) work without configuration

### Jellyfin

Configure Open Subtitles plugin credentials: Admin → Plugins → Open Subtitles → API key

### Seerr

Trigger initial library scan

### Sonarr / Radarr

Enable extension protection per indexer (UI): Settings → Indexers → edit each indexer → Advanced → Fail Downloads → enable Dangerous Extensions and Executable Extensions

### Syncthing

Accept device connections: approve pending devices on first sync

### Radicale

Share CalDAV/CardDAV URL with clients: `dav.<domain>` with generated `htpasswd` credentials

### WireGuard

Import client configs on devices
