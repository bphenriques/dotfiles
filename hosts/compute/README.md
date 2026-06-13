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

Guests are managed imperatively to keep things simpler:

```bash
pocket-id-manage guest invite someone@work.com --firstName John --lastName Doe
wg-manage add john colleague@work.com
```
