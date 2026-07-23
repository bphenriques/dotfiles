{
  _module.args.cvVm = {
    dataRoot = "/var/lib/cv";     # state volume: SSH host key (doubles as the sops age identity)
    tunnelPort = 8081;            # cloudflared -> Traefik
    staticPort = 8085;            # darkhttpd, localhost-only
    traefikMetricsPort = 9117;
  };
}
