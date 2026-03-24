# Install

## Prerequisites

Ensure the following are ready before installation:

| Dependency   | What                                                   | Reference                                                  |
|--------------|--------------------------------------------------------|------------------------------------------------------------|
| SMB Server   | Access to a SMB server                                 | [storage](../storage.md)                                   |
| Cloudflare   | DNS zone + API token (DNS-01 ACME challenge)           | [infrastructure](../infrastructure.md)                     |
| SMTP         | Account credentials for transactional email            | —                                                          |
| Backblaze B2 | Bucket + application key for off-site backups          | —                                                          |
| ZBT-2 Dongle | Nabu Casa Connect ZBT-2 (Thread/Matter radio)          | Flash OpenThread RCP firmware post-install                 |
| Secrets      | Bootstrap via `dotfiles-secrets init-host` (Bitwarden) | [`apps/nixos-install`](../../apps/nixos-install/README.md) |

## Installation

Follow [`apps/nixos-install`](../../apps/nixos-install/README.md) as it automates disk partitioning (disko), secrets
provisioning, and NixOS installation.

## Post-Install

While I aim to automate as much as possible, it is unwise to do so for everything. It is a balancing act between API
stability, maintenance, and declarative setup:

- Prefer stable API/CLI when possible. Home Assistant does not have an API and is heavily based on wizards.
- If I really need to access internal conventions, I need to ensure it is stable enough and worth it (e.g., Jellyfin and
  Jellyseerr). Opted out of Tandoor's Django ORM internals.
- Small one-time settings can be deferred post-install.

### Pocket-ID

Register admin passkey via browser (accept the invite received via email)

### Home Assistant

1. Complete onboarding wizard
2. Flash ZBT-2 with OpenThread RCP firmware using [Device Toolbox](https://toolbox.openhomefoundation.org/home-assistant-connect-zbt-2/) from a PC with Chrome
3. Add OTBR integration: Settings → Integrations → OTBR → `http://127.0.0.1:8091`
4. Add Thread integration: set OTBR network as preferred, enable Android/iOS credentials
5. Add Matter integration: accept default websocket URL
6. Sync Thread credentials in companion app, then commission Matter devices by scanning QR codes
7. Configure backup scheduler (UI): write backups to `/var/lib/hass/backups/` for off-site pickup

### Jellyfin

Configure Open Subtitles plugin credentials: Admin → Plugins → Open Subtitles → API key

### Jellyseerr

Trigger initial library scan

### Cleanuparr

Configure Radarr/Sonarr connections (UI): enter API keys and URLs

### Tandoor

Create guest/family users: Admin → Users and assign the space to the desired group

### Syncthing

Accept device connections: approve pending devices on first sync

### Radicale

Share CalDAV/CardDAV URL with clients: `dav.<domain>` with generated `htpasswd` credentials

### WireGuard

Import client configs on devices

### Obsidian LiveSync

Configure plugin in Obsidian clients: install the plugin and configure connections
