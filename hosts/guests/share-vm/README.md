# Share VM

A **sealed** cloud-hypervisor microVM on [`compute`](../../compute) for password-protected file sharing with people outside the fleet, exposed to the internet **only** via Tailscale Funnel.

Security concerns:
- No filesystem shared with compute — the VM owns its data on its own block devices
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

1. **Tailscale** (admin console): policy `{ "tagOwners": {"tag:share":["autogroup:admin"]}, "nodeAttrs": [{"target":["tag:share"],"attr":["funnel"]}] }`; enable **DNS → HTTPS Certificates**; generate a **reusable, non-ephemeral, `tag:share`** auth key.
2. **dotfiles-private** `hosts/share-vm/`: `settings.nix` (folders/users) + `secrets.yaml` with the auth key as `tailscale/authkey`, authored against `&base-microvm` (the bootstrap microvm key). `nix run .#host-secrets share-vm` prints the skeleton.
3. **Deploy.** Boots but Tailscale stays logged out — its key isn't a sops recipient yet.
4. Re-encrypt secrets to the VM's host key (it doubles as its sops age identity): `ssh compute ssh-keyscan -t ed25519 share-vm | awk '/ssh-ed25519/{print $2, $3}' | nix run nixpkgs#ssh-to-age`, add to `dotfiles-private/.sops.yaml` as `- &share-vm <age>` (keep `&base-microvm`, add to the share-vm key_group), `sops updatekeys`, commit.
5. **Deploy** again — secrets now decrypt.
6. **Approve** the new host in Tailscale, **restart** the VM, then confirm `ssh share-vm tailscale status`.
7. **Cloudflare**: a *proxied* (orange-cloud) A record `share` → `192.0.2.1`, plus a Static **302** redirect `share.<domain>` → `https://<vm>.<tailnet>.ts.net/files/`.
