# Host-local infra constants, exposed via _module.args so every share-vm module receives
# them as a `{ shareVm, ... }` argument (no file imports, single source for the mount paths).
{
  _module.args.shareVm = {
    dataRoot = "/var/lib/share";  # state volume: creds + SSH host key (sops age identity)
    filesRoot = "/srv/share";     # data volume: the shared files
    proxyPort = 8080;             # Traefik web entrypoint; the Funnel forwards here
  };
}
