{
  _module.args.shareVm = {
    dataRoot = "/var/lib/share";  # state volume: creds + SSH host key (sops age identity)
    filesRoot = "/srv/share";     # data volume: the shared files
    proxyPort = 8080;             # Traefik web entrypoint; the Funnel forwards here
    traefikMetricsPort = 9117;
  };
}