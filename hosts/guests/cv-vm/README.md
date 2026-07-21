# CV VM

A **sealed** cloud-hypervisor microVM on [`compute`](../../compute) serving a static CV landing page publicly via Tailscale Funnel.

Security concerns:
- Rate-limited to 5 req/s per client
- Egress is internet-only, never the LAN

## Ops

`ssh -J compute bphenriques@cv-vm`, then:

```bash
tailscale down | up           # kill switch: take the Funnel offline / back (resets on reboot)
tailscale funnel status       # current Funnel state
```

## Setup (one-time)

1. **Tailscale** (admin console): policy `{ "tagOwners": {"tag:cv":["autogroup:admin"]}, "nodeAttrs": [{"target":["tag:cv"],"attr":["funnel"]}] }`; generate a **reusable, non-ephemeral, `tag:cv`** auth key.
2. **dotfiles-private** `hosts/cv-vm/`: `secrets.yaml` with the auth key as `tailscale/authkey`, authored against `&base-microvm` (the bootstrap microvm key). `nix run .#host-secrets cv-vm` prints the skeleton.
3. **Deploy.** Boots but Tailscale stays logged out — its key isn't a sops recipient yet.
4. Re-encrypt secrets to the VM's host key (it doubles as its sops age identity): `ssh compute ssh-keyscan -t ed25519 cv-vm | awk '/ssh-ed25519/{print $2, $3}' | nix run nixpkgs#ssh-to-age`, add to `dotfiles-private/.sops.yaml` as `- &cv-vm <age>` (keep `&base-microvm`, add to the cv-vm key_group), `sops updatekeys`, commit.
5. **Deploy** again — secrets now decrypt.
6. **Approve** the new host in Tailscale, **restart** the VM, then confirm `ssh cv-vm tailscale status`.
