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
      } // lib.optionalAttrs service.ingress.enable {
        href = service.publicUrl;
        siteMonitor = "${service.publicUrl}${service.healthcheck.path}";
      } // lib.optionalAttrs (service.integrations.homepage.icon != null) {
        icon = service.integrations.homepage.icon;
      } // service.integrations.homepage.extraConfig;
    };

  mkExternalEntry = entry:
    {
      "${entry.name}" = {
        inherit (entry) description;
        href = entry.url;
      } // lib.optionalAttrs (entry.icon != null) {
        icon = entry.icon;
      };
    };

  servicesByCategory = lib.groupBy (s: s.category) homepageServices;
  externalByCategory = lib.groupBy (e: e.category) (lib.attrValues cfg.external);
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
    default = let
      services = lib.mapAttrs (_: svcs: map mkServiceEntry svcs) servicesByCategory;
      external = lib.mapAttrs (_: entries: map mkExternalEntry entries) externalByCategory;
    in lib.zipAttrsWith (_: lib.concatLists) [ services external ];
    readOnly = true;
    description = "Auto-generated homepage services grouped by category (read-only)";
  };
}
