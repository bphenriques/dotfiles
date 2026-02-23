The auth VM runs Pocket-ID (OIDC provider) as an isolated microvm.
- **Network**: Isolated bridge `br-auth` (10.20.0.0/24)
- **VM IP**: 10.20.0.10

## First-Time Setup (SOPS Bootstrap)

The VM auto-generates its age key on first boot. It will fail to decrypt (expected).
1. Extract VM's Age Public Key: `ssh auth 'cat /var/lib/sops-nix/key.txt' | age-keygen -y`
2. Copy this public key and add key to `.sops.yaml`
3. Re-encrypt Secrets: `sops updatekeys hosts/auth/secrets.yaml`
4. Redeploy

## Operations

Quick:
- SSH Access: `ssh auth  # Uses ~/.ssh/config matchBlock`. If it complains, likely because a new key was generated when rebooting: `ssh-keygen -R "[10.20.0.10]:2222"`
- Logs: `journalctl -u microvm@auth.service -f`
- Restart: `sudo systemctl restart microvm@auth`

###  Backup

Critical data to backup:
- `/var/lib/pocket-id/` - Pocket-ID database and encryption key
- `/var/lib/sops-nix/key.txt` - VM's age private key (or re-bootstrap if lost)

### Key Rotation
1. SSH into VM and generate new key:
   ```bash
   age-keygen -o /var/lib/sops-nix/key.txt
   ```
2. Get new public key: `age-keygen -y /var/lib/sops-nix/key.txt`
3. Update `.sops.yaml` with new key
4. Re-encrypt: `sops updatekeys hosts/auth/secrets.yaml`
5. Redeploy
