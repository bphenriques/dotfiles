# CV VM

A **sealed** cloud-hypervisor microVM on [`compute`](../../compute) serving a static CV landing page via a Cloudflare Tunnel.

Security concerns:

- Rate-limited to 5 req/s per client
- Egress is internet-only, never the LAN
  `cloudflared`: no inbound port and no home IP exposed.

## Ops

`ssh -J compute bphenriques@cv-vm`, then:

```bash
systemctl stop|start cloudflared  # kill switch: make network access offline / online
```

## Setup (one-time)

1. **Terraform**: `tofu apply` in [`infra`](../../../infra) creates the tunnel + apex DNS, then `tofu output -raw tunnel_token`.
2. **dotfiles-private** `hosts/cv-vm/secrets.yaml`: the token as `cloudflared/token`, authored against `&base-microvm` (the bootstrap microvm key). `nix run .#host-secrets cv-vm` prints the skeleton.
3. **Deploy.** Boots but the secret can't decrypt yet, so cloudflared stays down.
4. Re-encrypt secrets to the VM's host key (it doubles as its sops age identity): `ssh compute ssh-keyscan -t ed25519 cv-vm | awk '/ssh-ed25519/{print $2, $3}' | nix run nixpkgs#ssh-to-age`, add to `dotfiles-private/.sops.yaml` as `- &cv-vm <age>` (keep `&base-microvm`, add to the cv-vm key_group), `sops updatekeys`, commit.
5. **Deploy** again. The token decrypts, cloudflared connects, `https://bphenriques.com` goes live.
