# Inky

[InkyPi](https://github.com/fatihak/InkyPi) e-ink dashboard and [MPD](https://www.musicpd.org/) music player on a Raspberry Pi Zero 2W connected to an Inky Impression 7.3" screen and to a small DAC.

Given that the SD Card is not accessible, and it is not the best fit for NixOS, this system runs **Raspberry Pi OS Lite** which is modified using an idempotent imperative installation.

```
                                  ┌──────────────────────────┐
  ┌──────────────────┐   SMB      │          Inky            │
  │       NAS        │◀───────────┤                          │
  │   (music files)  │ (read only)│  MPD ──▶ DAC ◀))         │
  └──────────────────┘            │  (music player) (speaker)│
                                  │                          │
  ┌──────────────────┐  WireGuard │  WireGuard (backup VPN)  │
  │  Remote client   │◀──────────▶│  wg0 ──▶ LAN             │
  └──────────────────┘   (tunnel) └──────────────────────────┘
```

- **MPD** on inky plays music from the NAS (SMB mount) and outputs to the DAC
- **myMPD** on compute provides a web UI to control MPD remotely
- **WireGuard** backup VPN server

## Setup

**1. Flash SD Card** with `Raspberry Pi OS Lite (64-bit)` using [Raspberry Pi Imager](https://www.raspberrypi.com/software/):
```bash
nix-shell -p rpi-imager
sudo -E env QT_QPA_PLATFORM=wayland rpi-imager
```

In the imager settings:
- Hostname: `inky`
- User: `bphenriques` with a password
- SSH: enable with public key authentication
- WiFi: SSID and password
- Locale/timezone: `en_GB.UTF-8` / `Europe/Lisbon`

**2. Deploy and Configure**:

2.1: Add the credentials files:
```bash
ssh bphenriques@inky
sudo nano /root/.smb-credentials   # NAS username/password
sudo nano /etc/inkypi.env          # IMMICH_KEY
sudo mkdir -p /etc/wireguard
sudo nano /etc/wireguard/endpoint  # public IP/DDNS:port (e.g., vpn.example.com:51821)
```

2.2. Run the script:
```bash
nix build .#inky-setup
rsync -aL --delete result/ bphenriques@inky:/home/bphenriques/inky-setup/
ssh -t bphenriques@inky 'sudo bash /home/bphenriques/inky-setup/setup.sh'
```

**3. WireGuard (backup VPN)**:

Set up a port forward on the router (UDP `51821 → 192.168.1.92:51821`).

Show admin client QR code via SSH:
```bash
sudo wg-show
```

Notes
- Set up InkyPI Hardware Buttons plugin manually using the MPC wrapper: `/usr/local/bin/mpd-ctl {toggle|radio|vol-up|vol-down}`
- To regenerate the admin client keys: `sudo rm -rf /var/lib/wireguard/admin /etc/wireguard/wg0.conf` then re-run setup