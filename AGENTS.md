# Agent Instructions

NixOS/Darwin dotfiles flake. See [README.md](./README.md) for overview.

## Naming Conventions

- **Homelab namespace**: `custom.homelab.*`
- **Systemd setup units**: `*-configure.nix` files define `<service>-configure` units
- **Bitwarden secrets (NixOS)**: `system-nixos-<host>` (e.g., `system-nixos-compute`)

## Code Style

- Keep it simple and idiomatic—avoid fancy constructs
- Inline host-specific logic rather than creating non-reusable modules
- Single-responsibility modules; split into schema + consumers when needed
