# service-catalogue

Prints a Markdown service catalogue for a NixOS host by reading `config.custom.homelab.catalogue`.

```bash
nix run .#service-catalogue -- compute > hosts/compute/services.md
```
