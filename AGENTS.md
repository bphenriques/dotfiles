# Agent Instructions

NixOS/Darwin dotfiles flake. See [README.md](./README.md) for overview.

## Naming Conventions

- **Homelab namespace**: `custom.homelab.*`
- **Systemd setup units**: `*-configure.nix` files define `<service>-configure` units
- **Bitwarden secrets (NixOS)**: `system-nixos-<host>` (e.g., `system-nixos-compute`)

## Code Style

- Keep it simple and idiomatic—avoid fancy constructs
- Prefer architecturally sound and idiomatic solutions over quick shortcuts
- Ensure consistency with existing patterns—read neighboring files before writing new code
- Inline host-specific logic rather than creating non-reusable modules
- Single-responsibility modules; split into schema + consumers when needed

## Shell Scripts

- Use `pkgs.writeShellApplication` to wrap shell scripts with dependencies declared via `runtimeInputs`
- Keep logic in standalone `.sh` files (no shebang—they run under `writeShellApplication`'s strict mode)
- Do not use `pkgs.writeShellScriptBin` for internal scripts that shouldn't be on PATH

## Backup Hooks

- See [hosts/compute/README.md](./hosts/compute/README.md) for backup architecture and conventions

## Security Model

- See [hosts/compute/README.md](./hosts/compute/README.md) for access control tiers and secret management
