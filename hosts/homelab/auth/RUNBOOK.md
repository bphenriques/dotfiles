# Auth VM Runbook

## Overview

The auth VM runs Pocket-ID (OIDC provider) as an isolated microvm. It's deployed from the host (compute/laptop) and uses a dedicated age key for secrets decryption.

## Architecture

- **Host**: compute (production) or laptop (testing)
- **Network**: Isolated bridge `br-auth` (10.20.0.0/24)
- **VM IP**: 10.20.0.10
- **SSH**: Port 2222 (internal only)
- **Persistent storage**: `/var` mounted from `auth.img` volume

## First-Time Setup (SOPS Bootstrap)

The VM auto-generates its age key on first boot. It will fail to decrypt (expected).

Follow these steps to enable secrets decryption:
1. Extract VM's Age Public Key: `ssh auth 'cat /var/lib/sops-nix/key.txt' | age-keygen -y`
2. Copy this public key and add Key to .sops.yaml
3. Re-encrypt Secrets: `sops updatekeys hosts/auth/secrets.yaml`
4. Redeploy

## Operations

### SSH Access

```bash
ssh auth  # Uses ~/.ssh/config matchBlock
```

If it complains, likely because a new key was generated when rebooting: ssh-keygen -R "[10.20.0.10]:2222" 

### View VM Console/Logs

```bash
journalctl -u microvm@auth.service -f
```

### Restart VM

```bash
sudo systemctl restart microvm@auth
```

### Check Service Status (inside VM)

```bash
ssh auth
systemctl status pocket-id
systemctl status traefik
journalctl -u pocket-id -f
```

### View Decrypted Secrets (inside VM)

```bash
ssh auth
ls /run/secrets/
```

## Troubleshooting

### VM unreachable via network

1. Check bridge exists: `ip addr show br-auth`
2. Check TAP attached: `ip addr show vm-auth`
3. Check routing: `ip route get 10.20.0.10`
4. Check VM running: `systemctl status microvm@auth`

### Secrets not decrypting

1. Verify key exists: `ssh auth 'ls -la /var/lib/sops-nix/key.txt'`
2. Verify key in .sops.yaml: Check `hosts/auth/secrets.yaml` recipients
3. Re-encrypt if needed: `sops updatekeys hosts/auth/secrets.yaml`

### Services failing inside VM

```bash
ssh auth
systemctl status <service>
journalctl -u <service> --no-pager
```

## Backup

Critical data to backup:
- `/var/lib/pocket-id/` - Pocket-ID database and encryption key
- `/var/lib/sops-nix/key.txt` - VM's age private key (or re-bootstrap if lost)

## Key Rotation

If you need to rotate the VM's age key:

1. SSH into VM and generate new key:
   ```bash
   age-keygen -o /var/lib/sops-nix/key.txt
   ```
2. Get new public key: `age-keygen -y /var/lib/sops-nix/key.txt`
3. Update `.sops.yaml` with new key
4. Re-encrypt: `sops updatekeys hosts/auth/secrets.yaml`
5. Redeploy
