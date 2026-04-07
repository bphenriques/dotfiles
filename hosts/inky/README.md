# Inky

[InkyPi](https://github.com/fatihak/InkyPi) e-ink dashboard and [MPD](https://www.musicpd.org/) music player on a Raspberry Pi Zero 2W.

Runs **Raspberry Pi OS Lite** (not NixOS). `nix build .#inky-setup` produces an idempotent bootstrap bundle that configures the OS and installs InkyPi imperatively.

## Hardware

- **Board**: Raspberry Pi Zero 2W (512MB RAM)
- **Display**: Inky Impression 7.3" (SPI)
- **Audio**: MAX98357A I2S DAC

## Setup

**1. Flash SD Card**

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

**2. Deploy and Configure**

```bash
nix build .#inky-setup
rsync -a --delete result/ bphenriques@inky:/home/bphenriques/inky-setup/
ssh -t bphenriques@inky 'sudo bash /home/bphenriques/inky-setup/setup.sh'

# Set credentials (prompted by setup.sh on first run)
ssh bphenriques@inky
sudo nano /root/.smb-credentials   # NAS username/password
sudo nano /etc/inkypi.env          # IMMICH_KEY
```

## Notes

- MPD control: `/usr/local/bin/mpd-ctl {toggle|radio|vol-up|vol-down}`, for use with the InkyPi Hardware Buttons plugin.
