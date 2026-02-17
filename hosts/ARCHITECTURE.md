# Hosts Architecture

## Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                           Internet                                  │
└───────────────────────────────┬─────────────────────────────────────┘
                                │
                    ┌───────────▼───────────┐
                    │      Cloudflare       │
                    │  (proxy, WAF, DDoS)   │
                    └───────────┬───────────┘
                                │
              ┌─────────────────┴─────────────────┐
              │                                   │
              ▼                                   ▼
┌─────────────────────────┐         ┌─────────────────────────┐
│     auth.domain.com     │         │     *.domain.com        │
│                         │         │                         │
│  ┌───────────────────┐  │         │  ┌───────────────────┐  │
│  │   auth VM         │  │         │  │   compute host    │  │
│  │   (microvm)       │  │         │  │                   │  │
│  │                   │  │         │  │   Traefik :443    │  │
│  │   Traefik :443    │  │         │  │        │          │  │
│  │        │          │  │         │  │        ▼          │  │
│  │        ▼          │  │         │  │   home services   │  │
│  │   Pocket-ID       │  │   rsync │  │   - miniflux      │  │
│  │        │          │──┼─────────┼─▶│   - jellyfin      │  │
│  │        ▼          │  │  (6h)   │  │   - immich        │  │
│  │   OIDC creds      │  │         │  │   - etc...        │  │
│  └───────────────────┘  │         │  └───────────────────┘  │
└─────────────────────────┘         └─────────────────────────┘
```

## Hosts

| Host | Type | Purpose |
|------|------|---------|
| `laptop` | Physical | Personal workstation with GUI |
| `compute` | Physical | Headless home server running services |
| `auth` | MicroVM | Isolated OIDC provider (Pocket-ID) — **internet-facing** |
| `work-macos` | Darwin | Work machine (macOS) |

---

## Host: compute

**Location**: `hosts/compute/`

Main home server running containerized services behind Traefik reverse proxy.

### Key Components
- **Traefik**: Reverse proxy for `*.domain.com` with Cloudflare DNS ACME
- **Home services**: miniflux, jellyfin, immich, radarr, sonarr, etc.
- **MicroVM host**: Runs the auth VM
- **OIDC sync**: Pulls credentials from auth VM every 6 hours

### Network
- Static IP: `192.168.1.192`
- Bridge `br-auth`: `10.20.0.1/24` (for auth VM)

---

## Host: auth (MicroVM)

**Location**: `hosts/vm/auth/`

Isolated VM running Pocket-ID for OIDC authentication. **Exposed to the internet** as external auth provider for Tailscale and family use.

### Why a Separate VM?

| Option | Isolation | Complexity | Verdict |
|--------|-----------|------------|---------|
| **MicroVM** | VM (separate kernel) | Medium | ✅ Chosen |
| NixOS containers | Container (shared kernel) | Medium | ❌ Weaker isolation |
| Hardened systemd service | Process sandbox | Low | ❌ Insufficient for internet-facing |
| Separate physical machine | Full | High | ❌ Overkill for homelab |

**Rationale**:
1. **Internet-facing**: Compromise should not give access to entire homelab
2. **High-value target**: OIDC provider controls access to all services
3. **Blast radius**: VM escape is harder than container escape

### microvm.nix Dependency

**Trade-off accepted**:
- ✅ Declarative VM definition in flake
- ✅ NixOS-native integration
- ✅ Well-maintained by community
- ⚠️ External dependency (not in nixpkgs)

**Mitigation**: Pinned in flake.lock; VM config is portable if migration needed.

### Network Architecture

```
Internet → Cloudflare (proxy) → Router → Auth VM (10.20.0.10)
                                              │
                                              ├── Traefik (TLS, rate limiting)
                                              └── Pocket-ID
```

- **Cloudflare proxy**: Hides origin IP, DDoS protection
- **Rate limiting**: 50 req/s to prevent brute-force
- **Internal bridge**: VM on private 10.20.0.0/24, not directly on LAN

### OIDC Credential Provisioning

```
1. pocket-id-configure.service (auth VM)
   └─► Manages users and groups only

2. homelab-oidc-provision.service (compute, on boot)
   └─► Calls Pocket-ID API to create/update OIDC clients
   └─► Regenerates secrets and writes to /run/homelab-oidc/{client}/ (tmpfs)

3. Services on compute depend on homelab-oidc-ready.target
```

**Design**: Credentials are ephemeral (tmpfs) and regenerated on boot. For 24/7 server, this means rotation only on rare reboots. Manual rotation: `systemctl restart homelab-oidc-provision`.

### Security Layers

| Layer | Protection |
|-------|------------|
| Cloudflare | DDoS, WAF, origin IP hiding |
| VM Firewall | Only 80/443 open |
| Traefik | Rate limiting, TLS |
| fail2ban | SSH brute-force protection |
| SSH | Key-only, local network only |
| VM Isolation | Separate kernel, limited blast radius |
| Passkeys | Phishing-resistant, no passwords |

### Security Assessment

| Layer | Status | Notes |
|-------|--------|-------|
| Network isolation | ✅ | VM with separate kernel |
| TLS | ✅ | Traefik + ACME via Cloudflare DNS |
| Brute-force protection | ✅ | Rate limiting (50 req/s) |
| DDoS protection | ⚠️ | Requires Cloudflare proxy enabled |
| Origin hiding | ⚠️ | Requires Cloudflare proxy enabled |
| SSH | ✅ | Key-only, local network only |
| Authentication | ✅ | Passkeys (phishing-resistant) |

### Threat Model

| Scenario | Impact | Mitigation |
|----------|--------|------------|
| Pocket-ID vulnerability | Auth compromise | Keep updated, VM limits blast radius |
| Origin IP discovered | Bypass Cloudflare | Restrict firewall to Cloudflare IPs |
| VM disk failure | Auth data lost | Regular backups of `/var/lib/pocket-id/` |
| Home internet down | Can't authenticate | Accept risk (homelab limitation) |
| Family member locked out | Can't access services | Email-based recovery |

### Operational Requirements

1. **Backups**: `/var/lib/pocket-id/` (encryption key, database)
2. **Email**: SMTP must work for account recovery
3. **Availability**: Single point of failure; acceptable for homelab
4. **Updates**: Keep Pocket-ID updated for security patches

### Future Considerations

- **CrowdSec**: Traefik bouncer for automatic IP blocking
- **Cloudflare Authenticated Origin Pulls**: mTLS to prevent direct origin access

---

## Host: laptop

**Location**: `hosts/laptop/`

Personal workstation with full desktop environment.

### Key Components
- GUI environment (Wayland/niri)
- Development tools
- Can run local homelab services for testing (imports compute service configs)

---

## Directory Structure

```
hosts/
├── ARCHITECTURE.md           # This file
├── compute/                   # Main home server
│   ├── bphenriques/           # User config
│   ├── datastores/            # Database configs
│   ├── hardware/              # Hardware-specific
│   ├── services/              # Service definitions
│   ├── tasks/                 # Scheduled tasks
│   ├── vm/
│   │   └── microvm.nix        # MicroVM host setup
│   ├── config.nix
│   ├── default.nix
│   └── secrets.yaml
├── laptop/                    # Personal workstation
├── vm/                        # Virtual machines
│   └── auth/                  # Auth VM (Pocket-ID)
│       ├── services/pocket-id/
│       ├── config.nix
│       ├── default.nix
│       ├── secrets.yaml
│       ├── SESSION.md         # Implementation notes
│       └── TODO.md            # Setup checklist
└── work-macos/                # Work machine (Darwin)
```

## Module Dependencies

### compute
```
nixosModules (all) + sops + disko + home-manager + microvm.host
```

### auth (VM)
```
config/nixos/headless/external.nix + sops + microvm.microvm
```

Note: auth VM intentionally does NOT load home-server modules to avoid unnecessary dependencies.
