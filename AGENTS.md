# Agent Instructions

NixOS dotfiles flake. See [README.md](./README.md) for overview.

## Naming Conventions

- **Systemd setup units**: `*-configure.nix` files define `<service>-configure` units

## Code Style

- Keep it simple and idiomatic—avoid fancy constructs
- Prefer architecturally sound and idiomatic solutions over quick shortcuts
- Ensure consistency with existing patterns—read neighboring files before writing new code
- Inline host-specific logic rather than creating non-reusable modules
- Single-responsibility modules; split into schema + consumers when needed
- Accepted duplication (do not DRY): the per-service `*-configure` oneshot scaffolding and the nushell `wait_ready`/status-check helpers are intentionally repeated per service, not abstracted into a shared builder/lib. Keep them inline.

## Comments

- Default to none. If the code is clear it gets no comment; every comment must earn its place.
- Add one only for what the code can't say: the *why*, a non-obvious constraint, or a cross-file pointer. Never restate clear code, echo an option's name as a label, or repeat architecture/design that belongs in a README.
- When you do comment, one line and succinct—cut every word the sentence survives without.
- Architecture and design choices (the why/how of a host or subsystem, the threat model, the network posture) live in that host's `README.md`, not in module headers. A module header is at most a one-line orientation plus a pointer to the README.

## Shell Scripts

- Use `pkgs.writeShellApplication` to wrap shell scripts with dependencies declared via `runtimeInputs`
- Keep logic in standalone `.sh` files (no shebang—they run under `writeShellApplication`'s strict mode)
- Do not use `pkgs.writeShellScriptBin` for internal scripts that shouldn't be on PATH

## Backup Hooks

- See [hosts/compute/README.md](./hosts/compute/README.md) for backup architecture and conventions

## Security Model

- See [hosts/compute/README.md](./hosts/compute/README.md) for access control tiers and secret management
