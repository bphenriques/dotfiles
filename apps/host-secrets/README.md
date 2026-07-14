# host-secrets

Prints an example `secrets.yaml` for a NixOS host by reading `config.sops.secrets` and rendering nested YAML from `/`-delimited keys.

```bash
nix run .#host-secrets -- compute > dotfiles-private/hosts/compute/secrets.yaml
sops encrypt -i dotfiles-private/hosts/compute/secrets.yaml
```
