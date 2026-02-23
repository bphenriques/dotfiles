# Gateway VM

Edge reverse proxy running Pangolin with identity-aware access control.
Uses Pocket-ID (`auth` VM) as the single OIDC provider.

- **Network**: Bridge `br-microvm` (10.20.0.0/24)
- **VM IP**: 10.20.0.11
- **Dashboard**: https://pangolin.<domain>

## Architecture

```
Internet → Cloudflare → Compute Host → Gateway VM (Pangolin) → Services
                              ↓
                        Auth VM (Pocket-ID) for OIDC
```

**Components** (all managed by NixOS `services.pangolin`):
- **Pangolin**: Identity-aware proxy orchestrator
- **Traefik**: SSL termination (managed by Pangolin)
- **Gerbil**: WireGuard tunneling (port 51820)

**Traffic flow**:
1. Cloudflare terminates public SSL, forwards to compute host
2. Compute host NAT forwards to gateway VM
3. Pangolin handles routing, auth, and proxying to backends
4. Auth checks go to Pocket-ID (internal, not exposed)

## Compute Host Port Forwarding

The compute host needs to forward external traffic to the gateway VM:

```nix
# In hosts/homelab/compute/networking.nix or similar:
networking.nat.forwardPorts = [
  { destination = "10.20.0.11:443"; proto = "tcp"; sourcePort = 443; }
  { destination = "10.20.0.11:443"; proto = "udp"; sourcePort = 443; }
  { destination = "10.20.0.11:51820"; proto = "udp"; sourcePort = 51820; }
];
```

Note: Port 80 is NOT forwarded (SSL only).

## First-Time Setup

### 1. SOPS Bootstrap

The VM auto-generates its age key on first boot. It will fail to decrypt (expected).

```bash
# Extract VM's Age Public Key
ssh gateway 'cat /var/lib/sops-nix/key.txt' | age-keygen -y

# Update .sops.yaml: replace &gateway-vm placeholder
# Re-encrypt secrets
sops updatekeys hosts/homelab/gateway/secrets.yaml

# Redeploy
```

### 2. Create Secrets

```bash
sops hosts/homelab/gateway/secrets.yaml

# Required keys:
# pangolin:
#   server-secret: <openssl rand -base64 32>
# cloudflare:
#   dns-api-token: <API token with DNS:Edit permission>
```

### 3. Pangolin Initial Setup (One-Time)

1. Access `https://pangolin.<domain>/auth/initial-setup`
2. Create admin account
3. Create organization (e.g., "Homelab")

### 4. Create Pocket-ID OIDC Client

The OIDC client is already configured in `hosts/homelab/compute/services/pangolin-oidc.nix`.

After deploying compute host, credentials are provisioned to `/run/homelab-oidc/pangolin/`:
```bash
# On compute host
cat /run/homelab-oidc/pangolin/id      # Client ID
cat /run/homelab-oidc/pangolin/secret  # Client Secret
```

### 5. Add Pocket-ID as Identity Provider in Pangolin

Via Pangolin UI (Server Admin → Identity Providers):

1. Add Identity Provider → OAuth2/OIDC
2. Configure:
   - Name: `Pocket-ID`
   - Client ID: (from `/run/homelab-oidc/pangolin/id`)
   - Client Secret: (from `/run/homelab-oidc/pangolin/secret`)
   - Authorization URL: `https://auth.<domain>/authorize`
   - Token URL: `https://auth.<domain>/api/oidc/token`
   - Scopes: `openid profile email groups`
   - Identifier Path: `preferred_username`
3. Enable "Auto Provision Users"
4. Copy Redirect URL, verify it matches the callback URL in Pocket-ID

### 6. Add Resources

**Auth service** (reverse proxy to Pocket-ID):
- Subdomain: `auth`
- Target: `http://10.20.0.10:8082`
- Authentication: Public (login page must be accessible)

**Romm** (requires `users` group):
- Subdomain: `romm`
- Target: `http://10.20.0.1:8095` (compute host bridge IP)
- Authentication: Platform SSO, require group `users`

Note: Romm also validates OIDC groups internally (defense in depth).

## Integration API

The Integration API is enabled at `https://api.<domain>/v1/docs`.

After initial setup, you can automate resource creation:

```bash
# Create API key in Server Admin → API Keys (Root key)
# Then use API for:
# - Creating identity providers: PUT /idp/oidc
# - Creating resources: PUT /org/{orgId}/resource
# - Managing access: POST /resource/{resourceId}/roles
```

TODO: Create `pangolin-provision` service for declarative resource management.

## Operations

### SSH Access
```bash
ssh gateway  # Uses ~/.ssh/config
# If host key changed: ssh-keygen -R "[10.20.0.11]:2222"
```

### Logs
```bash
# On compute host
journalctl -u microvm@gateway.service -f

# On gateway VM
journalctl -u pangolin -f
journalctl -u traefik -f
journalctl -u gerbil -f
```

### Restart
```bash
sudo systemctl restart microvm@gateway  # On compute host
```

## Backup

Critical data:
- `/var/lib/pangolin/` - Database and config
- `/var/lib/pangolin/config/letsencrypt/` - SSL certificates
- `/var/lib/sops-nix/key.txt` - VM's age private key

## Security

### Firewall Rules
- **SSH (2222)**: Internal subnet only (10.20.0.0/24)
- **HTTPS (443)**: Cloudflare IPs + internal subnet
- **HTTP (80)**: Blocked entirely (SSL only)
- **WireGuard (51820)**: Open for Gerbil tunnels

### Access Control
- All resources require OIDC authentication via Pocket-ID
- Group-based policies: e.g., `romm` requires `users` group
- Defense in depth: apps validate OIDC groups directly

### Cloudflare IP Maintenance
The firewall rules include hardcoded Cloudflare IP ranges:
- Source: https://www.cloudflare.com/ips/
- Update: `hosts/homelab/networking.nix`

## Troubleshooting

### Certificate issues
```bash
journalctl -u traefik | grep -i acme
# Verify DNS API token has Zone:DNS:Edit permission
```

### OIDC failures
```bash
# Test gateway can reach auth VM
curl -v https://auth.<domain>/.well-known/openid-configuration

# Check /etc/hosts override
grep auth /etc/hosts
```

### Resources not accessible
1. Check resource status in Pangolin dashboard
2. Verify target is reachable from gateway VM
3. Check user has required group membership
