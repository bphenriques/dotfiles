{ lib, config, ... }:
let
  cfg = config.custom.homelab;

  visibleServices = lib.filterAttrs (_: s: s.integrations.catalogue.enable) cfg.services;

  mkEntry = service:
    let
      hasOidc = (lib.attrByPath [ "oidc" "enable" ] false service) == true;
      hasForwardAuth = service.forwardAuth.enable;
    in {
      inherit (service) name description category subdomain port version homepage;
      displayName = service.integrations.catalogue.displayName;
      auth = { oidc = hasOidc; forwardAuth = hasForwardAuth; };
      scope =
        if hasForwardAuth then service.forwardAuth.group
        else if hasOidc then cfg.groups.users
        else null;
    };
in
{
  config.custom.homelab._serviceOptionExtensions = [
    ({ name, ... }: {
      options.integrations.catalogue = {
        enable = lib.mkEnableOption "service catalogue entry" // {
          default = true;
        };

        displayName = lib.mkOption {
          type = lib.types.str;
          default = name;
          description = "Human-readable service name (defaults to attribute name)";
        };
      };
    })
  ];

  options.custom.homelab.catalogue = lib.mkOption {
    type = lib.types.attrsOf (lib.types.attrsOf lib.types.unspecified);
    default = lib.mapAttrs (_: mkEntry) visibleServices;
    readOnly = true;
    description = "Auto-generated service catalogue keyed by service name (read-only)";
  };
}
