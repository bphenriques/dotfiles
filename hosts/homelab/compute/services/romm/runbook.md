# RomM Runbook

## Architecture

RomM uses:
- **Native NixOS MariaDB** (`services.mysql`) for the database
- **Socket authentication** - no database password needed
- **Podman container** for the RomM application

## Admin User

An admin user is automatically created on first boot via `romm-configure.service`.
- Username: `admin`
- Password: stored in `/var/lib/romm/credentials/admin-password`

Check status: `systemctl status romm-configure`

### Rotating Admin Password

```bash
rm /var/lib/romm/credentials/admin-password
systemctl restart romm-configure
```

## Secret Rotation

### Application Secret (ROMM_AUTH_SECRET_KEY)

The auth secret is auto-generated on first boot and stored in `/var/lib/romm/secrets.env`.

```bash
systemctl stop podman-romm
rm /var/lib/romm/secrets.env
systemctl start podman-romm
```

## Troubleshooting

### Check service status

```bash
systemctl status podman-romm mysql romm-configure
```

### View logs

```bash
journalctl -u podman-romm -f
journalctl -u romm-configure -f
journalctl -u mysql -f
```

### Database connection issues

```bash
# Verify MySQL is running and socket exists
systemctl status mysql
ls -la /run/mysqld/mysqld.sock

# Test database connection
sudo -u romm mysql -u romm romm -e "SELECT 1"

# Check RomM user and database exist
sudo mysql -e "SELECT user, host FROM mysql.user WHERE user='romm'"
sudo mysql -e "SHOW DATABASES LIKE 'romm'"
```

### Container cannot access MySQL socket

```bash
# Verify romm user is in mysql group
id romm

# Check socket permissions
ls -la /run/mysqld/
```

## Data Locations

| Path | Description |
|------|-------------|
| `/var/lib/romm/credentials/admin-password` | Admin password |
| `/var/lib/romm/secrets.env` | Auth secret |
| `/var/lib/mysql` | MariaDB data (managed by NixOS) |
| `/var/lib/romm/resources` | RomM resources |
| `/var/lib/romm/redis` | Redis data |
| `/var/lib/romm/assets` | RomM assets |

## Backup

### Database

```bash
sudo mysqldump romm > romm-backup.sql
```

### Restore

```bash
sudo mysql romm < romm-backup.sql
```
