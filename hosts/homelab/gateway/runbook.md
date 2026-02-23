# Gateway VM Runbook

Manual steps required for initial deployment.

## Prerequisites

- [ ] Auth VM deployed and Pocket-ID accessible
- [ ] Compute/laptop host deployed with `pangolin-oidc.nix`
- [ ] DNS configured: `pangolin.<domain>` → Cloudflare → compute host IP

---

## Step 1: Create Secrets

```bash
# Generate server secret
openssl rand -base64 32

# Create secrets file
sops hosts/homelab/gateway/secrets.yaml
```

Required keys:
```yaml
pangolin:
  server-secret: "<generated-secret>"
cloudflare:
  dns-api-token: "<cloudflare-api-token-with-dns-edit>"
```

---

## Step 2: First Boot & SOPS Bootstrap

After first deployment, the gateway VM generates its age key but fails to decrypt secrets (expected).

```bash
# SSH into gateway (will fail if secrets not yet working, use console)
ssh gateway

# Or from compute host
sudo machinectl shell gateway

# Extract the public key
cat /var/lib/sops-nix/key.txt | age-keygen -y
```

Update `.sops.yaml`:
```yaml
keys:
  - &gateway-vm age1<actual-public-key-here>
```

Re-encrypt and redeploy:
```bash
sops updatekeys hosts/homelab/gateway/secrets.yaml
# Rebuild and deploy
```

---

## Step 3: Pangolin Initial Setup

**One-time only** - creates admin account.

1. Open `https://pangolin.<domain>/auth/initial-setup`
2. Create admin account:
   - Email: your email
   - Password: strong password
3. Create organization:
   - Name: `Homelab` (or your preference)

---

## Step 4: Add Pocket-ID as Identity Provider

### 4.1 Get OIDC Credentials

On compute/laptop host:
```bash
cat /run/homelab-oidc/pangolin/id      # Client ID
cat /run/homelab-oidc/pangolin/secret  # Client Secret
```

### 4.2 Configure in Pangolin

1. Go to **Server Admin** → **Identity Providers**
2. Click **Add Identity Provider**
3. Select **OAuth2/OIDC**
4. Configure:

| Field | Value |
|-------|-------|
| Name | `Pocket-ID` |
| Client ID | (from step 4.1) |
| Client Secret | (from step 4.1) |
| Authorization URL | `https://auth.<domain>/authorize` |
| Token URL | `https://auth.<domain>/api/oidc/token` |
| Scopes | `openid profile email groups` |
| Identifier Path | `preferred_username` |

5. Enable **Auto Provision Users**
6. Click **Create Identity Provider**
7. Copy the **Redirect URL** shown

### 4.3 Verify Pocket-ID Callback

The callback URL should match what's configured in `pangolin-oidc.nix`:
```
https://pangolin.<domain>/api/v1/auth/oidc/callback
```

If different, update `pangolin-oidc.nix` and redeploy compute host.

---

## Step 5: Configure Auto-Provisioning (Optional)

To map Pocket-ID groups to Pangolin roles:

1. In Pangolin, go to the Identity Provider settings
2. Configure **Auto Provision** rules:
   - Map `groups` claim to organization roles
   - Example: users in `admin` group get Admin role

---

## Step 6: Add Resources

### Auth Service (Pocket-ID UI)

1. Go to **Resources** → **Add Resource**
2. Configure:

| Field | Value |
|-------|-------|
| Name | `auth` |
| Subdomain | `auth` |
| Target | `http://10.20.0.10:8082` |
| Authentication | Disabled (public login page) |

### Romm (Users Group Only)

1. Go to **Resources** → **Add Resource**
2. Configure:

| Field | Value |
|-------|-------|
| Name | `romm` |
| Subdomain | `romm` |
| Target | `http://10.20.0.1:8095` |
| Authentication | Platform SSO |
| Access Policy | Require group `users` |

---

## Step 7: Test Access

1. Open `https://romm.<domain>` in incognito
2. Should redirect to Pangolin login
3. Click Pocket-ID login
4. Authenticate with passkey
5. Should redirect to Romm if user is in `users` group

---

## Verification Checklist

- [ ] `https://pangolin.<domain>` loads dashboard
- [ ] Pocket-ID IdP shows as connected
- [ ] Test user can login via Pocket-ID
- [ ] Resources show correct status
- [ ] Access policies work (users group can access, others denied)

---

## Troubleshooting

### Certificate Issues
```bash
# On gateway VM
journalctl -u traefik | grep -i acme
journalctl -u traefik | grep -i error
```

### OIDC Connection Issues
```bash
# On gateway VM - test connectivity to auth
curl -v https://auth.<domain>/.well-known/openid-configuration

# Check /etc/hosts
grep auth /etc/hosts
# Should show: 10.20.0.10 auth.<domain>
```

### Pangolin Not Starting
```bash
# On gateway VM
journalctl -u pangolin -f
journalctl -u gerbil -f

# Check secrets are decrypted
ls -la /run/secrets/
```

### Port Forwarding Issues
```bash
# On compute/laptop host
sudo nft list ruleset | grep -A5 prerouting

# Test from external machine
curl -v https://<host-ip>:443 --resolve pangolin.<domain>:443:<host-ip>
```
