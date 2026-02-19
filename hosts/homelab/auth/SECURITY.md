# Auth VM Security Model

This document describes the security model for the auth VM running Pocket-ID as an OIDC provider for Tailscale authentication.

## Threat Model

**Use case**: Personal/family OIDC provider for Tailscale SSO
- Limited users: owner, spouse, occasional close friends
- Invite-only registration with passkeys (no passwords)
- Self-hosted on residential connection behind Cloudflare

**Crown jewels**: If this VM is compromised, an attacker could mint Tailscale tokens and access the private network.

## Defense Layers

### 1. Network Layer

| Control | Status | Description |
|---------|--------|-------------|
| Cloudflare Proxy | ✅ | Origin IP hidden, DDoS protection |
| OS Firewall | ✅ | Port 443 restricted to Cloudflare IPs + internal subnet |
| Internal Bridge | ✅ | VM isolated on 10.20.0.0/24, not directly on LAN |
| SSH Non-standard Port | ✅ | Port 2222, internal access only |

### 2. Application Layer

| Control | Status | Description |
|---------|--------|-------------|
| Traefik Reverse Proxy | ✅ | TLS termination, rate limiting |
| Internal/External Separation | ✅ | Admin API (`/api/*`) blocked externally |
| Public API Allowlist | ✅ | Only specific paths exposed (webauthn, profile-picture) |
| Passkey-only Auth | ✅ | No passwords, phishing-resistant |
| Invite-only Registration | ✅ | No open registration |
| Fail2ban | ✅ | SSH brute-force protection |

### 3. Infrastructure Layer

| Control | Status | Description |
|---------|--------|-------------|
| Minimal VM | ✅ | Only essential services (Pocket-ID, Traefik, SSH) |
| SOPS Secrets | ✅ | Encrypted at rest, decrypted at boot |
| Key-only SSH | ✅ | No password authentication |
| Automatic Updates | ⚠️ TODO | Enable unattended security updates |

## Traffic Flow

```
External Users:
  Internet → Cloudflare → (443/TCP) → Traefik → Pocket-ID
  - Only public OIDC endpoints accessible
  - /api/* blocked (except allowlisted paths)
  - Rate limited

Internal (OIDC Provisioning):
  Compute Host → (10.20.0.10:443) → Traefik → Pocket-ID
  - Full /api/* access for user/client management
  - IP allowlist enforced
```

## Firewall Rules

Port 443 is restricted to:
- Cloudflare IPv4 ranges (see config.nix)
- Cloudflare IPv6 ranges
- Internal subnet (10.20.0.0/24)

All other sources are dropped.

## Public API Endpoints

The following `/api/*` paths are accessible externally (protected by Pocket-ID's invite tokens):

- `/api/webauthn/*` - Passkey registration/authentication
- `/api/users/profile-picture` - User profile picture

**TODO**: Review Pocket-ID documentation to ensure this list is exhaustive for the registration flow.

## Future Hardening (Optional)

| Item | Priority | Notes |
|------|----------|-------|
| CrowdSec | Medium | Intelligent threat detection, community blocklists |
| Security Headers | Low | HSTS, CSP, X-Frame-Options (may be overkill) |
| Cloudflare WAF Rules | Low | Additional protection for login endpoints |
| Automatic Updates | Medium | Enable for security patches |

## Not Needed

| Item | Reason |
|------|--------|
| Cloudflare Zero Trust | Overkill for limited user base |
| Pangolin | Overkill, adds complexity |
| mTLS | Passkeys + invite-only sufficient |
| VPS | Self-hosting preferred, Cloudflare provides adequate protection |

## Maintenance

- **Cloudflare IPs**: Keep in sync with https://www.cloudflare.com/ips/
- **Pocket-ID Updates**: Monitor for security patches
- **Backup**: `/var/lib/pocket-id/`, `/var/lib/traefik/acme.json`, SOPS age key
- **Key Rotation**: See RUNBOOK.md for age key rotation procedure

## Incident Response

If compromise suspected:
1. Revoke all Tailscale device authorizations
2. Rotate Pocket-ID API key
3. Regenerate SOPS age key (see RUNBOOK.md)
4. Review Traefik/Pocket-ID logs for unauthorized access
5. Rebuild VM from clean state if needed

---

*Last reviewed: 2026-02-19*
