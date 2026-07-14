# Share VM

A **sealed** cloud-hypervisor microVM on [`compute`](../compute) for password-protected file
sharing with people outside the fleet, exposed to the internet **only** via Tailscale Funnel.
It owns its files and `/nix/store` on its own block devices — compute shares nothing in — so
the only ways in are the Funnel (behind Traefik BasicAuth) and admin SSH over the compute
bridge. Egress is internet-only, never the LAN. Per-concern detail lives next to the code.

## Access

`folders` (dirs under `/srv/share`) and the `users` granted some of them live privately in
`dotfiles-private/hosts/share-vm/settings.nix`. Each user is a BasicAuth account; isolation is
by credential + scope. **Read-only by default** (`readOnly = false` grants uploads); a lone
folder is the user's scope, several are bind-mounted into a per-user scope dir.

```nix
users = {
  alice = { folders = [ "bob" ]; };                              # read-only on bob
  bob   = { folders = [ "bob" "family" ]; readOnly = false; };   # read-write on both
};
```

## Ops

`ssh -J compute bphenriques@share-vm`, then:

```bash
sudo share-rotate <user>      # issue a one-time password (printed once)
tailscale down | up           # kill switch: take the public Funnel offline / back (resets on reboot)
tailscale funnel status       # current Funnel state
```

- **Add/remove a user or folder**: edit `settings.nix`, rebuild. Removed users drop (the DB is
  rebuilt each boot); a removed folder's data lingers (no auto-delete) — `rm` it via the mount.
- **Manage files**: the laptop automounts the share at `/mnt/homelab-shared-vm`; curate there,
  reflected to recipients live.

Metrics → compute's Grafana (**Share VM** dashboard).

## Setup (one-time)

1. **Tailscale** (admin console): policy `{ "tagOwners": {"tag:share":["autogroup:admin"]}, "acls": [], "nodeAttrs": [{"target":["tag:share"],"attr":["funnel"]}] }`; enable **DNS → HTTPS Certificates**; generate a **reusable, non-ephemeral, `tag:share`** auth key.
2. **dotfiles-private** `hosts/share-vm/`: `settings.nix` (folders/users) + `secrets.yaml` with the auth key as `tailscale/authkey`, authored against `&base-microvm` (your permanent microvm bootstrap key). `nix run .#host-secrets share-vm` prints the skeleton.
3. **Deploy.** The VM boots but Tailscale stays logged out — its key isn't a sops recipient yet.
4. **Register the VM's host key** (it doubles as the VM's sops age identity). Capture it over the compute bridge, register in both places, `sops updatekeys`, commit, redeploy, then confirm `ssh share-vm tailscale status`:
   ```bash
   key=$(ssh compute ssh-keyscan -t ed25519 share-vm | awk '/ssh-ed25519/{print $2, $3}')
   echo "$key"                                # → hosts/shared.nix   ssh.hostKeys.share-vm
   echo "$key" | nix run nixpkgs#ssh-to-age   # → dotfiles-private/.sops.yaml as `- &share-vm <age>` (keep &base-microvm), add to the share-vm key_group
   ```
5. **Cloudflare**: a *proxied* (orange-cloud) A record `share` → `192.0.2.1`, plus a Static **302** redirect rule `share.<domain>` → `https://<vm>.<tailnet>.ts.net/files/`.
