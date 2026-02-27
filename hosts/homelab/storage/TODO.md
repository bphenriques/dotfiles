# Storage (Synology NAS) TODO

## UPS Network Sharing for Compute Host

The UPS is connected via USB to the Synology NAS. To allow the `compute` host to gracefully shutdown on power failure, configure NUT (Network UPS Tools) in client/server mode.

### Synology Setup (NUT Server)

1. Go to **Control Panel → Hardware & Power → UPS**
2. Enable **"Enable network UPS server"**
3. Add the compute host IP to **"Permitted DiskStation devices"** (e.g., `192.168.1.X`)
4. Note the UPS name (typically `ups`)

### Compute Host Setup (NUT Client)

Add to the compute NixOS configuration:

```nix
power.ups = {
  enable = true;
  mode = "netclient";
  schedulerRules = ''
    [upsmon]
    MONITOR ups@192.168.1.192 1 upsmon secret slave
    SHUTDOWNCMD "/run/current-system/sw/bin/shutdown -h now"
  '';
};
```

Replace:
- `ups` with the UPS name from Synology
- `192.168.1.192` with the NAS IP
- `upsmon` / `secret` with credentials configured in Synology (if any)

### Testing

1. Verify connection: `upsc ups@192.168.1.192`
2. Check status: `upsmon -c reload`
3. Test shutdown (carefully): Unplug UPS briefly and verify compute receives shutdown signal
