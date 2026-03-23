# Install

## Prerequisites

Ensure the following are ready before installation:

| Dependency   | What                                                   | Reference                                                  |
|--------------|--------------------------------------------------------|------------------------------------------------------------|
| SMB Server   | Access to a SMB server                                 | [storage](../storage.md)                                   |
| Cloudflare   | DNS zone + API token (DNS-01 ACME challenge)           | [infrastructure](../infrastructure.md)                     |
| SMTP         | Account credentials for transactional email            | —                                                          |
| Backblaze B2 | Bucket + application key for off-site backups          | —                                                          |
| Secrets      | Bootstrap via `dotfiles-secrets init-host` (Bitwarden) | [`apps/nixos-install`](../../apps/nixos-install/README.md) |

## Installation

Follow [`apps/nixos-install`](../../apps/nixos-install/README.md) as it automates disk partitioning (disko), secrets provisioning, and NixOS installation.

## Post-Install

While I aim to automate as much as possible, it is unwise to do so for everything. It is a balancing act between API
stability, maintenance, and declarative setup:

- Prefer stable API/CLI when possible. Home Assistant does not have an API and is heavily based on wizards.
- If I really need to access internal conventions, I need to ensure it is stable enough and worth it (e.g., Jellyfin and
  Jellyseerr). Opted out of Tandoor's Django ORM internals.
- Small one-time settings can be deferred post-install.

| Service           | Action                                           | Notes                                                         |
|-------------------|--------------------------------------------------|---------------------------------------------------------------|
| Pocket-ID         | Register admin passkey via browser               | Accept the invite received via email address to setup account |
| Home Assistant    | Complete onboarding wizard                       | —                                                             |
| Home Assistant    | Configure backup scheduler (UI)                  | Write backups to `/var/lib/hass/backups/` for off-site pickup |
| Jellyfin          | Configure Open Subtitles plugin credentials (UI) | Admin → Plugins → Open Subtitles → API key                    |
| Jellyseerr        | Trigger initial library scan                     | —                                                             |
| Cleanuparr        | Configure Radarr/Sonarr connections (UI)         | Enter API keys and URLs in Cleanuparr settings                |
| Tandoor           | Create guest/family users (UI)                   | Admin → Users and assign the space to the desired group       |
| Syncthing         | Accept device connections                        | Approve pending devices on first sync                         |
| Radicale          | Share CalDAV/CardDAV URL with clients            | `dav.<domain>` with generated `htpasswd` credentials          |
| WireGuard         | Import client configs on devices                 | Import configurations                                         |
| Obsidian LiveSync | Configure plugin in Obsidian clients             | Install the plugin in Obsidian and connections                |
