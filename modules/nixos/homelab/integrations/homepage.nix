{ lib, config, ... }:
let
  cfg = config.custom.homelab;

  homepageServices = lib.filter
    (s: s.integrations.homepage.enable)
    (lib.attrValues cfg.services);

  mkServiceEntry = service:
    {
      "${service.name}" = {
        inherit (service) description;
        href = service.publicUrl;
        siteMonitor = service.publicUrl;
      } // lib.optionalAttrs (service.integrations.homepage.icon != null) {
        icon = service.integrations.homepage.icon;
      } // service.integrations.homepage.extraConfig;
    };

  servicesByCategory = lib.groupBy (s: s.category) homepageServices;
in
{
  config.custom.homelab._serviceOptionExtensions = [
    ({ name, ... }: {
      options.integrations.homepage = lib.mkOption {
        type = lib.types.submodule {
          options = {
            enable = lib.mkEnableOption "homepage entry for this service";

            icon = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              default = "${name}.svg";
              description = "Icon name from dashboard-icons (e.g. 'miniflux.svg')";
            };

            extraConfig = lib.mkOption {
              type = lib.types.attrs;
              default = { };
              description = "Extra homepage configuration (widgets, etc.)";
            };
          };
        };
        default = { };
        description = "Homepage dashboard integration";
      };
    })
  ];

  options.custom.homelab.homepage.generatedServices = lib.mkOption {
    type = lib.types.attrsOf (lib.types.listOf lib.types.anything);
    default = lib.mapAttrs (_: svcs: map mkServiceEntry svcs) servicesByCategory;
    readOnly = true;
    description = "Auto-generated homepage services grouped by category (read-only)";
  };
}
