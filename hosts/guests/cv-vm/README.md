# CV VM

A **sealed** cloud-hypervisor microVM on [`compute`](../../compute) serving a static CV landing
page publicly, exposed to the internet **only** via Tailscale Funnel. The site is baked into the
image (`/nix/store`) — compute shares nothing in — so the only ways in are the Funnel (Traefik →
darkhttpd, no auth) and admin SSH over the compute bridge. Rate-limited to 5 req/s per client;
egress is internet-only, never the LAN. Per-concern detail lives next to the code.

The page lives in [`services/landing/site/`](./services/landing/site) — edit and rebuild. A `curl`
of the site is an easter egg: signature response headers, `/humans.txt`, and a custom 404.

## Ops

`ssh -J compute bphenriques@cv-vm`, then:

```bash
tailscale down | up           # kill switch: take the public Funnel offline / back (resets on reboot)
tailscale funnel status       # current Funnel state
```

Public URL (behind the QR) → `https://cv-vm.<tailnet>.ts.net`. Metrics → compute's Prometheus
(node + Traefik). Fronting the URL with a vanity domain is done externally, not in this repo.

## Setup (one-time)

1. **Tailscale** (admin console): add `tag:cv` with the Funnel attr — policy
   `{ "tagOwners": {"tag:cv":["autogroup:admin"]}, "nodeAttrs": [{"target":["tag:cv"],"attr":["funnel"]}] }`;
   generate a **reusable, non-ephemeral, `tag:cv`** auth key. (HTTPS certificates already enabled for share-vm.)
2. **dotfiles-private** `hosts/cv-vm/secrets.yaml`: set the auth key as `tailscale/authkey` (replaces the
   placeholder), authored against `&base-microvm` (your permanent microvm bootstrap key) — `sops hosts/cv-vm/secrets.yaml`.
3. **Deploy** (`dot compute s`). The VM boots but Tailscale stays logged out — its key isn't a sops recipient yet.
4. **Register the VM's host key** (it doubles as the VM's sops age identity). Capture it over the compute
   bridge, register in both places, `sops updatekeys`, commit, redeploy, then confirm `ssh cv-vm tailscale status`:
   ```bash
   key=$(ssh compute ssh-keyscan -t ed25519 cv-vm | awk '/ssh-ed25519/{print $2, $3}')
   echo "$key"                                # → hosts/shared.nix   ssh.hostKeys.cv-vm
   echo "$key" | nix run nixpkgs#ssh-to-age   # → dotfiles-private/.sops.yaml as `- &cv-vm <age>` (keep &base-microvm), add to the cv-vm key_group
   ```
