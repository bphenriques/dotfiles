{ lib, config, ... }:
let
  cfg = config.custom.home-server;
in
{
  config = lib.mkIf cfg.enable {
    virtualisation = {
      podman.enable = true;
      oci-containers.backend = "podman"; # Rootful Podman is accepted as long as it is not publicly exposed
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
