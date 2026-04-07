# Inky

[InkyPi](https://github.com/fatihak/InkyPi) e-ink dashboard and [MPD](https://www.musicpd.org/) music player on a Raspberry Pi Zero 2W.

Runs **Raspberry Pi OS Lite** (not NixOS). Configuration is managed via an idempotent setup bundle built with Nix (`nix build .#inky-setup`).

## Hardware

- **Board**: Raspberry Pi Zero 2W (512MB RAM)
- **Display**: Inky Impression 7.3" (SPI)
- **Audio**: MAX98357A I2S DAC

## Setup

### 1. Flash SD Card

Use [Raspberry Pi Imager](https://www.raspberrypi.com/software/) to flash **Raspberry Pi OS Lite (64-bit)**.
```bash
nix-shell -p rpi-imager
sudo -E env QT_QPA_PLATFORM=wayland rpi-imager
```

In the imager settings (the setup script assumes these are done):
- Hostname: `inky`
- User: `bphenriques` with a password
- SSH: enable with public key authentication
- WiFi: SSID and password
- Locale/timezone: `en_GB.UTF-8` / `Europe/Lisbon`

### 2. Run Setup

```bash
nix build .#inky-setup
rsync -a --delete result/ bphenriques@192.168.1.92:/home/bphenriques/inky-setup/
ssh -t bphenriques@192.168.1.92 'sudo bash /home/bphenriques/inky-setup/setup.sh'
```

### 3. Configure SMB Credentials

```bash
ssh bphenriques@192.168.1.92
sudo nano /root/.smb-credentials   # Set actual NAS username/password
```

## Network Ports

| Port | Service       |
|------|---------------|
| 80   | InkyPi web UI |
| 6600 | MPD           |
