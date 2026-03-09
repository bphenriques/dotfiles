# Inky

Raspberry Pi Zero 2W with an e-ink display for ambient information and music playback.

## Hardware

- **Model**: Raspberry Pi Zero 2W
- **CPU**: ARM Cortex-A53 (quad-core)
- **RAM**: 512MB
- **Storage**: SD card
- **Display**: Inky Impression 7.3" (SPI)
- **Audio**: MAX98357A I2S DAC (hifiberry-dac overlay)
- **Network**: WiFi only (wlan0)

## Software

- **Display**: [InkyPi](https://github.com/fatihak/InkyPi) - web-based e-ink dashboard
- **Music**: [Music Player Daemon](https://www.musicpd.org/) with ALSA output
- **Discovery**: Avahi (mDNS) - accessible via `inky.local`

## Setup

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

## Notes

### SD Card Wear Reduction
- Logs stored in RAM (volatile journald)
- `/tmp` as tmpfs (64MB)
- Root filesystem: `noatime` + `commit=60`

### Power Optimization (~30-40% reduction)
- GPU frequency scales down to 50MHz when idle
- Activity LED disabled
- Bluetooth disabled
- CPU governor: `ondemand`

### Reliability
- Hardware watchdog enabled (30s timeout, 10min reboot)
- WiFi-only boot (doesn't wait for ethernet)
