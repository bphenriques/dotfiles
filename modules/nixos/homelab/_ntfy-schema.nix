# Per-service ntfy integration contract (options only).
# Imported by services-registry.nix and backup.nix, consumed by ntfy-sh host configuration.
{ lib, serviceName }:
let
  tokenDir = "/var/lib/homelab-secrets/ntfy-publishers";
in
{ ... }: {
  options = {
    topic = lib.mkOption {
      type = lib.types.str;
      description = "Notification topic this service publishes to";
    };

    tokenFile = lib.mkOption {
      type = lib.types.str;
      default = "${tokenDir}/${serviceName}";
      readOnly = true;
      description = "Path to the generated access token file for this publisher";
    };
  };
}
