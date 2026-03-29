# Inky

Raspberry Pi Zero 2W with Inky Impression 7.3" display powering [InkyPi](https://github.com/fatihak/InkyPi) and a small DAC (MAX98357A) for the [Music Player Daemon](https://www.musicpd.org/).

## Setup

### SD Card Installation

Build, flash, and provision secrets in one step:

```bash
sudo fdisk -l  # Identify the SD card device (e.g. /dev/sdX)
nix run .#nixos-install -- sd-card inky <bw-email> /dev/sdX
```

Insert the SD card into the Pi Zero 2W and power it on. It should connect to WiFi and be reachable at `inky.local` (mDNS) or `192.168.1.197` (static DHCP reservation).

### InkyPi Installation

InkyPi is installed imperatively due to mutable state (plugins, config):

```bash
cd /opt
sudo git clone https://github.com/fatihak/InkyPi.git inkypi
sudo chown -R inkypi:inkypi inkypi
cd inkypi
sudo -u inkypi python3 -m venv .venv
sudo -u inkypi .venv/bin/pip install -r requirements.txt
```

The systemd service activates automatically once `/opt/inkypi/server/server.py` exists.

### Verify Audio

After first boot, verify the I2S DAC is working:

```bash
aplay -l  # Should show "sndrpihifiberry"
```

### Network Ports

| Port | Service         |
|------|-----------------|
| 5000 | InkyPi web UI   |
| 6600 | MPD             |

