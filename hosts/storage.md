## Storage

Non-exhaustive documentation regarding how I am setting up my Synology DS923+. Backups are handled by the compute host, so they're out of scope here.

## Initial Setup

- Update to latest DSM 7.2.x.
- Keep `admin` and `guest` accounts disabled. Rotate `admin`'s password to something random anyway, in case the disable flag ever flips.

## Storage Pools

Two pools:

1. Storage Pool 1: SHR with 2 HDDs.
   1. Quick SMART test every month.
   2. Extended SMART test every 6 months.
2. Storage Pool 2: RAID 1 with two NVMe devices. Requires https://github.com/007revad/Synology_HDD_db.
   1. Trim every week.

## Volumes

Per volume, in Storage Manager:

- Schedule data scrubbing every 3 months, restricted to off-hours via the time grid. Catches bit-rot before btrfs can no longer repair it.
- Disable "Record File Access Time" under the volume's settings. Atime adds writes on every read for no real benefit.

## Shared Folders

| Folder        | Storage Type | Recycle Bin | Snapshot | Hidden     | Media | Description                                                           |
| ------------- | ------------ | ----------- | -------- | ---------- | ----- | --------------------------------------------------------------------- |
| `bphenriques` | HDD          | Yes         | Yes      | Restricted | No    | Private files.                                                        |
| `media`       | HDD          | Yes         | Yes      | No         | No    | Media files with no private information.                              |
| `shared`      | HDD          | Restricted  | Yes      | No         | Yes   | Shared files across all users.                                        |
| `homes`       | HDD          | Yes         | Yes      | Yes        | Yes   | Requires enabling under `Users & Group` -> `Advanced` -> `User Home`. |

When creating each folder, tick "Enable data checksum for advanced data integrity". Only settable at creation, and without it the scrub above can detect bit-rot but not repair it.

## Users

| User              | Type          | Extra Groups       | Services |
| ----------------- | ------------- | ------------------ | -------- |
| `guest`           | Disabled      | -                  | -        |
| `admin`           | Disabled      | -                  | -        |
| `Bruno-Admin`     | Admin         | -                  | All      |
| `Bruno`           | User          | `private`, `media` | Only SMB |
| `machine-compute` | Internal User | `private`, `media` | Only SMB |
| `machine-inky`    | Internal User | `media`            | Only SMB |

## Groups

| Group     | Description                                 |
| --------- | ------------------------------------------- |
| `private` | R+W permissions to private media files.     |
| `media`   | R+W permissions to non-private media files. |

## Snapshots

Under Snapshot Replication application, I set it across all folders. For example:

- Every 2h, starting at midnight.
- Retain all snapshots for 5 days.
- Retain the latest snapshot per day for 60 days.

And also enable the tickbox that makes the snapshots visible.

## Security

Under Control Panel -> Security:

- Account -> enable Adaptive MFA. Admins signing in from outside the LAN get a one-time code emailed to the linked Synology account.
- Protection -> enable Auto Block. 10 failed attempts in 5 minutes, unblock after 1 day.

## Firewall

1. Enable firewall.
2. Then add the rules:
   1. Private IPs (in order):
      1. Allow subnet `10.0.0.0` (subnet mask `255.0.0.0`)
      2. Allow subnet `192.168.0.0` (subnet mask `255.255.0.0`)
      3. Allow subnet `172.16.0.0` (subnet mask `255.240.0.0`)
   2. Disallow everything else. **it has to be the LAST rule**.

## SSH

1. Enable SSH and change port
2. Under `Users and Group` and then `advanced`, enable home directory. There should be two new folders: `home` and `homes`.
3. SSH to your machine: `ssh Bruno-Admin@{ip or host}`
4. Edit `/etc/ssh/sshd_config`: `sudo vim /etc/ssh/sshd_config`:
   1. Uncomment `#PubkeyAuthentication yes`
   2. Uncomment `#AuthorizedKeysFile  .ssh/authorized_keys`
5. Restart SSH: `sudo synoservicectl --reload sshd`.
6. Sign-off of your NAS SSH session and run: `ssh-copy-id -p <port> Bruno-Admin@{ip or host}` to copy our public key.

TODO: ideally disable password authentication (see [this link](https://www.cyberciti.biz/faq/how-to-disable-ssh-password-login-on-linux/)), and rely solely on SSH keys.

## Notifications

Under Control Panel -> Notification -> Email, send via the linked Synology account rather than a custom SMTP server. The NAS can still warn me when something breaks even if Gmail or another provider rotates its auth.

## Updates

Under Control Panel -> Update & Restore -> Update Settings, automatically install important (security) updates only. Feature upgrades stay manual.

## NUT Server

1. Go to Control Panel -> Hardware & Power -> UPS
2. Enable "Enable network UPS server"
3. Add the compute host IP to "Permitted DiskStation devices".

Also under Hardware & Power -> General, enable "Restart automatically when power supply issue is fixed" so the NAS comes back on its own after a brownout. Graceful shutdown is still handled via NUT.

On the client (compute host):

1. Verify connection: `upsc ups@192.168.1.192`
2. Check status: `systemctl status upsmon.service`

## Cron Jobs

- Daily cronjob that empties the recycle bins.
- Weekly script to trim the NVMe SSDs.

## Monitoring

Under Resource Monitor -> Settings, enable data usage history. Useful when debugging "why was the NAS slow at X" after the fact.
