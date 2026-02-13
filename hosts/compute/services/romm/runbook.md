# ROMM Runbook

## Secret Rotation

Secrets are auto-generated on first boot and stored in `/var/lib/romm/secrets.env`.

### Rotating Application Secrets (AUTH_SECRET, ADMIN_PASSWORD)

These can be rotated without database changes:

```bash
# Stop services
systemctl stop podman-romm podman-romm-db

# Remove secrets file
rm /var/lib/romm/secrets.env

# Regenerate and restart
systemctl start romm-pre-start-secrets podman-romm-db podman-romm
```

### Rotating Database Passwords

Database password rotation requires manual MariaDB intervention:

```bash
# 1. Stop the app container (keep DB running)
systemctl stop podman-romm

# 2. Connect to MariaDB and change password
podman exec -it romm-db mariadb -u root -p
# Then run: ALTER USER 'romm-user'@'%' IDENTIFIED BY 'new_password';

# 3. Update secrets file with new password
# Edit /var/lib/romm/secrets.env and update DB_PASSWD and MYSQL_PASSWORD

# 4. Restart services
systemctl restart podman-romm-db podman-romm
```

## Troubleshooting

### Check service status

```bash
systemctl status podman-romm podman-romm-db romm-pre-start-secrets
```

### View logs

```bash
journalctl -u podman-romm -f
journalctl -u podman-romm-db -f
```

### Network issues

```bash
# Verify network exists
podman network ls | grep romm-internal

# Recreate if needed
podman network rm romm-internal
systemctl restart podman-romm-db podman-romm
```

## Data Locations

| Path | Description |
|------|-------------|
| `/var/lib/romm/secrets.env` | Generated secrets |
| `/var/lib/romm/mysql` | MariaDB data |
| `/var/lib/romm/resources` | ROMM resources |
| `/var/lib/romm/redis` | Redis data |
| `/var/lib/romm/assets` | ROMM assets |
