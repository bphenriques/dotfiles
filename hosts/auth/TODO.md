# Auth VM Setup TODO

## Pre-Launch Checklist (Before Opening Ports)

- [ ] Enable Cloudflare proxy (orange cloud) for `auth.domain.com`
- [ ] Test login flow works end-to-end
- [ ] Test email delivery (invite/recovery emails)
- [ ] Test passkey registration on mobile + desktop
- [ ] Create accounts for family members
- [ ] Document recovery process for family (what to do if locked out)
- [ ] Set up backup for `/var/lib/pocket-id/` (encryption key, database)
- [ ] (Optional) Set up uptime monitoring (e.g., Uptime Kuma, healthchecks.io)

## Before Deployment

- [ ] Generate auth VM age key:
  ```bash
  age-keygen -o /var/lib/sops-nix/system-keys.txt
  ```
- [ ] Update `.sops.yaml`: Replace `age1xxxxxxxxx...` placeholder with actual key
- [ ] Encrypt `secrets.yaml` with actual secrets:
  - `pocket-id/api-key`
  - `pocket-id/smtp-password`
  - `cloudflare_dns_api_token`
- [ ] Update SSH authorized key in `config.nix` (line ~45) with your actual public key
- [ ] Add Pocket-ID API key to `compute/secrets.yaml` under `pocket-id/api-key` (same key as auth VM)
- [ ] Fix `system.stateVersion` in `config.nix` to actual NixOS release (e.g., `"24.11"`)

## Network Configuration

- [ ] Configure router/DNS to point `auth.domain.com` to the VM's external IP
- [ ] Ensure the VM is reachable via its hostname (Tailscale or local DNS)

## Cloudflare Configuration (Dashboard)

- [ ] Enable **Cloudflare Proxy** (orange cloud) for `auth.domain.com`
  - Hides origin IP from attackers
  - Provides DDoS protection
- [ ] Consider restricting origin firewall to Cloudflare IPs only:
  - https://www.cloudflare.com/ips/
  - This prevents direct access bypassing Cloudflare
- [ ] Enable WAF rules for login endpoints (optional)

## Future: CrowdSec Integration

Consider adding CrowdSec for advanced threat detection:
- [ ] Install CrowdSec in auth VM
- [ ] Add Traefik bouncer for automatic blocking
- [ ] Subscribe to community blocklists

See: https://www.crowdsec.net/
