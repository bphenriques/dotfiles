{ lib, config, ... }:
let
  cfg = config.custom.homelab;

  adminCategories = [ "Monitoring" "Administration" ];
  defaultTab = category: if lib.elem category adminCategories then "Admin" else "Home";

  homepageServices = lib.filter
    (s: s.integrations.homepage.enable)
    (lib.attrValues cfg.services);

  mkServiceEntry = service:
    {
      "${service.displayName}" = {
        inherit (service.metadata) description;
      } // lib.optionalAttrs service.ingress.enable {
        href = service.publicUrl;
        siteMonitor = "${service.publicUrl}${service.healthcheck.path}";
      } // lib.optionalAttrs (service.integrations.homepage.icon != null) {
        icon = service.integrations.homepage.icon;
      } // service.integrations.homepage.extraConfig;
    };

  mkExternalEntry = entry:
    {
      "${entry.displayName}" = {
        inherit (entry) description;
        href = entry.url;
      } // lib.optionalAttrs (entry.icon != null) {
        icon = entry.icon;
      };
    };

  servicesByTab = lib.groupBy (s: s.integrations.homepage.tab) homepageServices;
  externalsByTab = lib.groupBy (e: e.tab) (lib.attrValues cfg.external);

  mkTabServices = tab: let
    svcs = servicesByTab.${tab} or [];
    exts = externalsByTab.${tab} or [];
  in map mkServiceEntry svcs ++ map mkExternalEntry exts;
in
{
  config.custom.homelab._serviceOptionExtensions = [
    ({ name, config, ... }: {
      options.integrations.homepage = lib.mkOption {
        type = lib.types.submodule {
          options = {
            enable = lib.mkEnableOption "homepage entry for this service";

            tab = lib.mkOption {
              type = lib.types.enum [ "Home" "Admin" ];
              default = defaultTab config.metadata.category;
              description = "Homepage tab to display this service on";
            };

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

  options.custom.homelab.homepage = {
    generatedHomeServices = lib.mkOption {
      type = lib.types.listOf lib.types.anything;
      default = mkTabServices "Home";
      readOnly = true;
      description = "Auto-generated Home tab services as a flat list (read-only)";
    };

    generatedAdminServices = lib.mkOption {
      type = lib.types.listOf lib.types.anything;
      default = mkTabServices "Admin";
      readOnly = true;
      description = "Auto-generated Admin tab services as a flat list (read-only)";
    };
  };
}
