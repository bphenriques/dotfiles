# The sharing service, split per component (mirrors compute's selfhost/services layout):
# Traefik is the public edge, FileBrowser the app behind it, prometheus the host metrics
# (Traefik exposes its own HTTP metrics).
{ ... }:
{
  imports = [
    ./traefik.nix
    ./filebrowser
    ./prometheus.nix
  ];
}
