{
  # App owns OIDC wiring, CREATE_ADMIN and the local Postgres; defaults (port 8081, admin-only) are
  # overridable on selfhost.services.miniflux. Per-user reader preferences are a dotfiles concern living
  # in custom.users.<name>.selfhost.apps.miniflux, applied by ./configure.nix via the admin API.
  imports = [ ./configure.nix ];
  selfhost.apps.miniflux.enable = true;
}
