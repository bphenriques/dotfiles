# Share VM

A **sealed** cloud-hypervisor microVM on [`compute`](../../compute) for password-protected file sharing with people outside the fleet, exposed to the internet **only** via Tailscale Funnel.

Security concerns:

- No filesystem shared with compute; the VM owns its data on its own block devices
- Only ways in: the Funnel (behind Traefik BasicAuth) and admin SSH over the compute bridge
- Rate-limited to 30 req/s per client
- Egress is internet-only, never the LAN

## Ops

`ssh -J compute bphenriques@share-vm`, then:

```bash
sudo share-rotate <user>      # issue a one-time password (printed once)
tailscale down | up           # kill switch: take the Funnel offline / back (resets on reboot)
tailscale funnel status       # current Funnel state
```

## Access

Under `.dotfiles-private`, the access folders and the users are set as follows:

```nix
users = {
  alice = { folders = [ "bob" ]; };                              # read-only on bob
  bob   = { folders = [ "bob" "family" ]; readOnly = false; };   # read-write on both
};
```

Each user is a BasicAuth account that only has the target folders in scope.

`laptop` automounts the share at `/mnt/homelab-shared-vm`.

## Setup (one-time)

1. **Tailscale** (admin console). Add to the policy, enable **DNS → HTTPS Certificates**, and mint a **reusable, non-ephemeral `tag:share`** auth key:

   ```json
   "tagOwners": { "tag:share": ["autogroup:admin"] },
   "nodeAttrs": [{ "target": ["tag:share"], "attr": ["funnel"] }]
   ```

2. **dotfiles-private** `hosts/share-vm/`: `settings.nix` (folders/users) and `secrets.yaml` with the auth key as `tailscale/authkey`, authored against `&base-microvm`. `nix run .#host-secrets share-vm` prints the skeleton.

3. **Register the VM's sops identity.** Its host key only exists after first boot, so it takes two deploys:

   - **Deploy.** Boots logged-out; the key isn't a sops recipient yet.
   - `ssh compute ssh-keyscan -t ed25519 share-vm | awk '/ssh-ed25519/{print $2, $3}' | nix run nixpkgs#ssh-to-age`, add to `.sops.yaml` as `- &share-vm <age>` (share-vm key_group; keep `&base-microvm`), then `sops updatekeys` and commit.
   - **Deploy** again. Secrets now decrypt.

4. **Approve** the host in Tailscale, **restart** the VM, then confirm `ssh share-vm tailscale status`.

5. **Cloudflare** (in [`infra`](../../../infra)): add `share = "https://share-vm.<tailnet>.ts.net/files/"` to `redirects` in dotfiles-private, then `tofu apply` (proxied placeholder + 302 `share.<domain>` to the Funnel).
