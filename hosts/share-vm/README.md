# Share VM

Password-protected file sharing for one-off shares with people outside the fleet —
a **sealed** cloud-hypervisor microVM on [`compute`](../compute), exposed to the internet
**only** through Tailscale Funnel. Family file access lives on `compute`'s FileBrowser
(OIDC); this is its external-facing counterpart.

Treated as insecure by default: it owns its files and its `/nix/store` on its own
block devices, so compute shares **nothing** into it (no virtiofs, no NFS). The only
ways in are the public Funnel (app-authed) and admin SSH over the bridge.

## Architecture

```
  recipient ─▶ share.<domain>                    Cloudflare redirect → .../files/ (no origin)
            ─▶ <vm>.<tailnet>.ts.net/files/      Tailscale Funnel — TLS terminates on the VM
            ─▶ Traefik :8080                      BasicAuth (per-user) + per-IP rate limit
            ─▶ FileBrowser (localhost)            scope set by login, not URL

  /srv/share on a VM-owned block device           25 GB ext4 cap, noexec; no host share

  admin ─▶ ssh -J compute bphenriques@share-vm    SSH only over the compute bridge
  laptop ─▶ share-mount (sshfs over that SSH)      curate files, then move to the NAS
```

The relay only ever sees ciphertext (TLS terminates on the VM); the PROXY protocol
gives Traefik the real client IP for rate limiting. FileBrowser never touches the
network — Traefik is the only reverse proxy in front of it. On compute the VMM runs
unprivileged (`microvm:kvm`) and `ProtectSystem=strict`-confined to the VM's
own state, so even a hypervisor escape can't reach the rest of the host.

## Access

Defined in [`lib.nix`](./lib.nix) as two decoupled sets — `folders` (directories under
`/srv/share`) and `users` mapped onto them. A folder can be shared by several users, and
a username need not match a folder. Each user is a BasicAuth account; isolation is by
credential + scope, not URL.

```nix
folders = [ "bob" "family" ];
users = {
  bob     = { folder = "bob"; };                  # read-write on /srv/share/bob
  alice   = { folder = "bob"; readOnly = true; };  # read-only on the same folder
  partner = { folder = null; readOnly = true; };   # null → "/" — sees every folder
};
```

FileBrowser gives each user a single scope, so a user maps to one folder (or `/` for
all); several users can share a folder with different `readOnly`. Issue a password with
`share-manage rotate <user>`.

## Setup

One-time, outside this repo:

1. **dotfiles-private** — add `hosts.share-vm` with a `sopsSecretsFile`, and store the
   auth key from step 3 in it as `tailscale/authkey`.

2. **Tailscale — a locked-down, single-purpose tailnet** (admin console). Tailscale is
   used *only* for this Funnel, so the node gets zero tailnet reach:

   - **Access Controls** → replace the policy with:
     ```json
     {
       "tagOwners": { "tag:share": ["autogroup:admin"] },
       "acls": [],
       "nodeAttrs": [ { "target": ["tag:share"], "attr": ["funnel"] } ]
     }
     ```
     `"acls": []` is default-deny: the node can't reach the tailnet and the tailnet
     can't reach it. `nodeAttrs` grants nothing but Funnel.
   - **DNS → HTTPS Certificates → Enable** — Funnel needs the `*.ts.net` certificate.

3. **Auth key** — admin console → **Settings → Keys → Generate auth key**:
   **Reusable** ✅ · **Ephemeral** ❌ · **Tag** `tag:share`. Tagging applies the ACL
   above and disables key expiry (the server never deauths). Copy it into step 1.

4. **Cloudflare — redirect only, no origin.** Both parts are required: the rule only
   runs if Cloudflare actually receives the request, which needs a *proxied* DNS record.

   - **DNS → Records → Add:** type **A**, name **`share`**, IPv4 **`192.0.2.1`** (a
     reserved test address — never contacted), **Proxy status: Proxied (orange cloud)**.
     The orange cloud is the point: it routes the request to Cloudflare's edge so the
     rule below can fire *before* any origin is reached. A grey-cloud record would send
     traffic to the dead placeholder and skip the rule.
   - **Rules → Redirect Rules → Create:** match `Hostname equals share.<domain>`,
     **Static** redirect, **302**, target:
     ```
     https://<vm>.<tailnet>.ts.net/files/
     ```
     Redirect straight to `/files/` — FileBrowser serves its listing there, so landing
     on `/` would flash-redirect. The recipient's folder is set by their login (the
     password you issue), **not** the URL path, so a single static target serves everyone.
     Cloudflare returns the redirect at the edge; the `192.0.2.1` origin is never hit, and
     the relay only ever sees the redirect, never file content.

5. **No compute backup.** The VM's files are staging — curate the keepers onto the NAS
   (`share-mount`, then move), where the normal backup job already covers them. What
   stays on the VM is transient and re-sharable, so it needs no durable copy.

After the first deploy the node appears as `share-vm` (tagged `tag:share`) at
`<vm>.<tailnet>.ts.net` — use that URL for the redirect in step 4.

### Secrets bootstrap (two-phase)

The VM decrypts secrets with its own SSH host key (→ age), generated on first boot,
so it can only become a sops recipient *after* it exists:

1. With `tailscale/authkey` in its sops file, deploy. The VM boots and generates its
   host key, but Tailscale stays **logged out** (it can't yet decrypt the key).
2. Add the VM as a recipient and re-encrypt — from dotfiles-private:
   ```bash
   ssh share-vm sudo cat /var/lib/share/.ssh-host-keys/ssh_host_ed25519_key.pub | nix run nixpkgs#ssh-to-age
   # add that age1… to the share-vm rule in .sops.yaml, then:
   sops updatekeys <share-vm secrets file>
   ```
   Commit, bump the flake input, redeploy. Tailscale now authenticates — confirm with
   `ssh share-vm tailscale status`.

## Ops

Admin commands run via `ssh -J compute bphenriques@share-vm`:

```bash
sudo share-manage rotate <user>      # issue a fresh password — printed once, never stored
sudo share-manage funnel on|off      # manual kill switch / reopen
sudo share-manage funnel status      # Funnel URL / state
```

The **Funnel opens at boot and stays up.** The random per-user passwords are the
defence; rotate a user's password after a sharing burst to invalidate it, and
`share-manage funnel off` is the manual kill switch.

- **Add access**: add a `users` entry in `lib.nix` (and its `folder` to `folders` if new) and rebuild, then `share-manage rotate <user>` to issue the password.
- **Remove access**: delete the `users` entry and rebuild — the account is dropped (folder + data left in place).
- **Manage files**: `share-mount` on the laptop (sshfs at `~/mnt/share`, `share-umount` when done), reflected to recipients live.

Passwords are bcrypt-only at rest (htpasswd); the plaintext is shown once on `rotate` and never written. Metrics flow to compute's Grafana — see the **Share VM** dashboard.
