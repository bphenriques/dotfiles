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

- [ ] First boot: Deploy VM (secrets will fail, expected)
- [ ] Extract VM's age key: `ssh auth` then `age-keygen -y /var/lib/sops-nix/key.txt`
- [ ] Add VM's public key to `.sops.yaml` under `&auth-vm`
- [ ] Re-encrypt secrets: `sops updatekeys hosts/auth/secrets.yaml`
- [ ] Redeploy VM (secrets now work)
- [ ] Encrypt `secrets.yaml` with actual secrets:
  - `pocket-id/api-key`
  - `pocket-id/smtp-password`
  - `cloudflare_dns_api_token`
- [ ] Add Pocket-ID API key to `compute/secrets.yaml` under `pocket-id/api-key` (same key as auth VM)

See [RUNBOOK.md](./RUNBOOK.md) for detailed bootstrap steps.

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
