# Podman configuration with hardened defaults for all containers.
#
# Rootful Podman is acceptable here:
# - Services are not publicly exposed (behind Traefik on LAN)
# - Containers are hardened with restrictive defaults (no caps, pids limit, no-new-privileges)
# - Rootless adds complexity (UID mapping, credential access) without proportional benefit for homelab
#
# These defaults are stricter than Podman's out-of-box settings, which prioritize compatibility.
{ lib, config, ... }:
let
  cfg = config.custom.home-server;
in
{
  config = lib.mkIf cfg.enable {
    virtualisation = {
      podman.enable = true;
      oci-containers.backend = "podman";
      containers.containersConf.settings = {
        containers = {
          default_capabilities = [];  # Drop all capabilities by default; containers must add them explicitly
          pids_limit = 100;           # Prevent fork bombs from exhausting host PIDs
          no_new_privileges = true;   # Prevent privilege escalation via setuid binaries
        };
      };
    };
  };
}
