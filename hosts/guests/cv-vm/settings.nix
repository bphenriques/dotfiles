{
  _module.args.cvVm = {
    dataRoot = "/var/lib/cv";     # state volume: SSH host key (doubles as the sops age identity)
    proxyPort = 8080;             # Traefik web entrypoint; the Funnel forwards here
    staticPort = 8085;            # darkhttpd, localhost-only
    traefikMetricsPort = 9117;
  };
}
