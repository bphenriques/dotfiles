{ lib, config, ... }:
let
  cfg = config.custom.homelab;

  visibleServices = lib.filterAttrs (_: s: s.integrations.catalogue.enable) cfg.services;

  mkEntry = service:
    let
      hasOidc = lib.attrByPath [ "oidc" "enable" ] false service;
      hasForwardAuth = service.forwardAuth.enable;
    in {
      inherit (service) name subdomain port displayName;
      inherit (service.metadata) description category version homepage;
      auth = { oidc = hasOidc; forwardAuth = hasForwardAuth; };
      hasBackup = service.backup.package != null;
    };
in
{
  options.custom.homelab.catalogue = lib.mkOption {
    type = lib.types.attrsOf (lib.types.attrsOf lib.types.unspecified);
    default = lib.mapAttrs (_: mkEntry) visibleServices;
    readOnly = true;
    description = "Auto-generated service catalogue keyed by service name (read-only)";
  };
}
