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

BIOS tweaks to ensure thermal stability as sustained workloads caused thermal shutdowns (~84°C):

- **BIOS fan curve** (`Del` → `Advanced → Hardware Monitor → Smart Fan Function`): adjust to run fans at full-speed at **80°C** (default is 90°C) and to start earlier with slope of 4 PWM/°C. Temperatures dropped from 83°C to ~65°C under identical load.
- **Systemd `throttled.slice`**: heavy services (Immich, Jellyfin) pinned to cores 1-2 (`AllowedCPUs`), hard-capped (`CPUQuota=150%`).
- **Systemd `critical.slice`**: SSH/DHCP.

## Architecture

```
  Cloudflare           ┌──────────────────────────────────────────┐
  (DNS + ACME only)    │            Compute Server                │
                       │                                          │
  LAN / Wireguard ─────├──▶ Traefik ──┬──▶ Pocket-ID (OIDC)       │
                       │              ├──▶ Jellyfin, Immich, ...  │
                       │              ├──▶ Homepage               │
                       │              ├──▶ Alertmanager, ntfy     │
                       │              │                           │
                       │  Prometheus ─┼──▶ scrape targets         │
                       │       │      │                           │
                       │       ▼      │                           │
                       │  Alertmanager──▶ ntfy ──▶ push notif     │
                       └──────────────┼───────────────────────────┘
                                      │
                                      ▼
                                ┌───────────┐
                                │    NAS    │
                                │ (Synology)│
                                └───────────┘
                                  SMB mounts
```

- **[Cloudflare](../infrastructure.md)**: DNS (records point to internal IP; not proxied) + ACME DNS-01 for certificate issuance (Traefik terminates TLS locally)
- **Traefik**: Reverse proxy; only externally reachable entry point alongside Wireguard
- **Pocket-ID**: OIDC provider with passkey authentication
- **Tinyauth**: ForwardAuth middleware for services without native OIDC
- **Wireguard**: VPN for remote access
- **SMB**: Services access [NAS](../storage) storage via SMB mounts
- **Backblaze B2**: Off-site encrypted backups via [rustic](https://github.com/rustic-rs/rustic) (daily backup, weekly verification). Restore with `rustic restore latest <target-dir>` (see [rustic docs](https://rustic.cli.rs/docs/commands/restore.html)).
- **Prometheus + Alertmanager**: Metrics, HTTP probes, and alerting.
- **[ntfy](https://ntfy.sh)**: Push notifications for system alerts and service events

Services bind to `127.0.0.1` where possible; the firewall default-denies all other ports. Only Traefik and services that by nature require direct network access (e.g., Wireguard, Syncthing) are reachable on specific interfaces.

Installation: [here](install.md).

## Access Control

| Group    | Target          | Example access     |
| -------- |---------------------| ------------------ |
| `admin`  | Homelab owner       | Everything         |
| `users`  | Family              | Media, recipes     |
| `guests` | Friends, colleagues | Romm only (viewer) |

Guests are managed imperatively to keep things simpler:

```bash
pocket-id-manage guest invite someone@work.com --firstName John --lastName Doe
wg-manage add john colleague@work.com
```
