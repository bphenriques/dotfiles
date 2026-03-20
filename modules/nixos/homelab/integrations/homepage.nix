{ lib, config, ... }:
let
  cfg = config.custom.homelab;

  adminCategories = [ "Monitoring" "Administration" ];
  defaultTab = category: if lib.elem category adminCategories then "Admin" else "Home";

  scopeLabels = {
    everyone = "Everyone";
    family = "Family";
    admin = "Admin";
  };

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

  # Group by tab, then by scope (Home) or flat (Admin)
  servicesByTab = lib.groupBy (s: s.integrations.homepage.tab) homepageServices;
  externalsByTab = lib.groupBy (e: e.tab) (lib.attrValues cfg.external);

  mkHomeServices = let
    svcs = servicesByTab.${"Home"} or [];
    exts = externalsByTab.${"Home"} or [];
    servicesByScope = lib.mapAttrs (_: ss: map mkServiceEntry ss) (lib.groupBy (s: scopeLabels.${s.scope}) svcs);
    externalByScope = lib.mapAttrs (_: es: map mkExternalEntry es) (lib.groupBy (_: scopeLabels.everyone) exts);
  in lib.zipAttrsWith (_: lib.concatLists) [ servicesByScope externalByScope ];

  mkAdminServices = let
    svcs = servicesByTab.${"Admin"} or [];
    exts = externalsByTab.${"Admin"} or [];
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
              default = defaultTab config.category;
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
    scopeLabels = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = scopeLabels;
      readOnly = true;
      description = "Mapping from scope enum values to display labels (read-only)";
    };

    generatedHomeServices = lib.mkOption {
      type = lib.types.attrsOf (lib.types.listOf lib.types.anything);
      default = mkHomeServices;
      readOnly = true;
      description = "Auto-generated Home tab services grouped by scope label (read-only)";
    };

    generatedAdminServices = lib.mkOption {
      type = lib.types.listOf lib.types.anything;
      default = mkAdminServices;
      readOnly = true;
      description = "Auto-generated Admin tab services as a flat list (read-only)";
    };
  };
}
