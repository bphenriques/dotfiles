{
  # App owns the upstream, htpasswd-from-selfhost.users, DAV route and backup; per-user opt-ins live
  # in selfhost.users.<name>.apps.radicale.enable. Defaults (port 5232, admin-only, forwardAuth)
  # are overridable on selfhost.services.radicale.
  selfhost.apps.radicale.enable = true;
}
