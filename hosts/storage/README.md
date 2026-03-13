## Storage

Non-exhaustive documentation regarding how I am setting up my Synology DS923+.

## Hardrive setup

Nothing really relevant to say other than I have two storage pools:
1. Storage Pool 1: SHR with 2 HDDs.
   1. Quick SMART test every month
   2. Extended SMART test every 6 months.
2. Storage Pool 2: RAID 1 with two NVMe devices. Requires https://github.com/007revad/Synology_HDD_db.
   1. Trim every week.

## Users

| User             | Type          | Extra Groups                                         | Services |
|------------------|---------------|------------------------------------------------------|----------|
| `guest`          | Disabled      | -                                                    | -        |
| `admin`          | Disabled      | -                                                    | -        |
| `Bruno-Admin`    | Admin         | `docker`, `docker-data-private`, `docker-data-media` | All      |
| `Bruno`          | User          | `private`, `media`                                   | Only SMB |
| `docker-media`   | Internal User | `docker-data-media`                                  | None     |
| `docker-private` | Internal User | `docker-data-private`, `docker-data-media`           | None     |

Note: `Bruno-Admin` must share some groups with `docker` and `docker-private` in order to set group permissions correctly to run containers.

## Shared Folders

| Folder        | Storage Type | Recycle Bin | Snapshot | Hidden     | Media | Description                                                           |
|---------------|--------------|-------------|----------|------------|-------|-----------------------------------------------------------------------|
| `bphenriques` | HDD          | Yes         | Yes      | Restricted | No    | Private files.                                                        |
| `docker`      | NvME SSD     | No          | Yes      | Yes        | No    | Docker data files.                                                    |
| `media`       | HDD          | Yes         | Yes      | No         | No    | Media files with no private information.                              |
| `shared`      | HDD          | Restricted  | Yes      | No         | Yes   | Shared files across all users.                                        |
| `homes`       | HDD          | Yes         | Yes      | Yes        | Yes   | Requires enabling under `Uses & Group` -> `Advanced` -> `User Home`.. |

## Groups

| Group                 | Docker Data | Private Media | Media | Description                                                                              |
|-----------------------|-------------|---------------|-------|------------------------------------------------------------------------------------------|
| `docker`              | No          | No            | No    | Group to call `docker` without `sudo`. I am still running `docker` in a root context.    |
| `docker-data-media`   | Yes         | No            | Yes   | R+W permissions to docker data directory and media files that do not handle private data |
| `docker-data-private` | Yes         | Yes           | Yes   | R+W permissions to docker data directory  and media files that handle private data       |
| `private`             | No          | Yes           | No    | R+W permissions to private media files.                                                  |
| `media`               | No          | No            | Yes   | R+W permissions to non-private media files.                                              |

## Snapshots

Under Snapshot Replication application, I set it across all folders. For example:
- Every 2h at midnight.
- Retain all snapshots for 5 days.
- Retain the latest snapshot per day for 60 days.

And also enable the tickbox that makes the snapshots visible.

## Cron Jobs

- Default cron jobs that checks the disks (quick and extended SMART tests).
- Daily cronjob that empties the recycle bins.
- Weekly script that updates the server (this requires setting up a `renovate` token):
```
source /var/services/homes/Bruno-Admin/.bashrc
home-server task update
home-server task renovate
```

- Weekly script to trim my SSD.

## SSH

1. Enable SSH and change port
2. Under `Users and Group` and then `advanced`, enable home directory. There should be two new folders: `home` and `homes`.
3. SSH to your machine: `ssh Bruno-Admin@{ip or host}`
4. Edit `/etc/ssh/sshd_config`: `sudo vim etc/ssh/sshd_config`:
   1. Uncomment `#PubkeyAuthentication yes`
   2. Uncomment `#AuthorizedKeysFile  .ssh/authorized_keys`
5. Restart SSH: `sudo synoservicectl --reload sshd`.
6. Sign-off of your NAS SSH session and run: `ssh-copy-id -p <port> Bruno-Admin@{ip or host}` to copy our public key.

TODO: ideally disable password authentication (see [this link](https://www.cyberciti.biz/faq/how-to-disable-ssh-password-login-on-linux/)), and rely solely on SSH keys.

## Firewall

1. Enable firewall.
2. Then add the rules:
   1. Private IPs (in order):
      1. Allow subnet `10.0.0.0` (subnet mask `255.0.0.0`)
      2. Allow subnet `192.168.0.0` (subnet mask `255.255.0.0`)
      3. Allow subnet `172.16.0.0` (subnet mask `255.240.0.0`)
   2. Disallow everything else. **it has to be the LAST rule**.

## Misc

- Disable automatically power on when the power is restored.
