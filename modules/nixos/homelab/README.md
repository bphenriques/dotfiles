# Homelab Module

Declarative service management framework for NixOS. A single `custom.homelab.services.<name>` declaration drives routing, auth, secrets, monitoring, homepage entries, backup hooks, and systemd ordering.

See [`hosts/compute/README.md`](../../../hosts/compute/README.md) for usage examples and architecture.

## Structure

```
├── services-registry.nix        # Service registry: routing, metadata, auth, backup hooks
├── tasks-registry.nix           # Task registry: timers, maintenance jobs (non-HTTP)
├── users.nix                    # User declarations: identity, groups, per-service flags
├── ingress.nix                  # Traefik reverse proxy, derived from service declarations
├── backup.nix                   # Backup lifecycle: hooks, retention, verification
├── smb.nix                      # SMB/CIFS mounts with systemd dependency wiring
├── paths.nix                    # Storage path conventions (media, per-user)
├── media.nix                    # Media quality profile schemas (Radarr/Sonarr)
├── smtp.nix                     # SMTP configuration options
├── security/
│   ├── secrets.nix              # Runtime secret generation + template rendering
│   └── oidc.nix                 # OIDC client provisioning from service declarations
├── integrations/
│   ├── homepage.nix             # Homepage dashboard entries, derived from services
│   ├── monitoring.nix           # Per-service healthcheck monitoring flag
│   ├── ntfy.nix                 # Push notification integration for services/tasks
│   └── catalogue.nix            # Auto-generated service catalogue
└── user-services/               # Per-user option extensions (e.g., jellyfin.enable per user)
```

## Extension Mechanism

The registries (`services`, `tasks`, `users`) use `submoduleWith` with deferred module extensions. Integration modules append options to the base schema via internal lists:

- `_serviceOptionExtensions` — extended by: `security/oidc.nix`, `security/secrets.nix`, `backup.nix`, `integrations/homepage.nix`, `integrations/monitoring.nix`, `integrations/ntfy.nix`, `integrations/catalogue.nix`, and each `user-services/*.nix`
- `_taskOptionExtensions` — extended by: `security/secrets.nix`, `integrations/ntfy.nix`
- `_userOptionExtensions` — extended by: `security/oidc.nix`, and each `user-services/*.nix`

This follows the open-closed principle: new integrations extend the schema without modifying the core registries.

## Placeholder Formats

Templates use two intentionally distinct placeholder formats, each owned by a different subsystem:

- **Secrets** (`__HOMELAB_SECRET_<owner>_<name>__`): locally generated secrets, owned by `security/secrets.nix`
- **OIDC** (`@HOMELAB_OIDC_<service>_<field>@`): credentials from the external OIDC provider, owned by `security/oidc.nix`

Different delimiters make it immediately clear which subsystem owns each placeholder when reading a template. Both are substituted at runtime via `replace-secret` with unreplaced-placeholder detection as a safety net.
