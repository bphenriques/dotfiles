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
        inherit (service.integrations.homepage) icon;
      } // service.integrations.homepage.extraConfig;
    };

  mkExternalEntry = entry:
    {
      "${entry.displayName}" = {
        inherit (entry) description;
        href = entry.url;
      } // lib.optionalAttrs (entry.icon != null) {
        inherit (entry) icon;
      };
    };

  servicesByTab = builtins.groupBy (s: s.integrations.homepage.tab) homepageServices;
  externalsByTab = builtins.groupBy (e: e.tab) (lib.attrValues cfg.external);

  mkTabServices = tab: let
    svcs = servicesByTab.${tab} or [];
    exts = externalsByTab.${tab} or [];
  in map mkServiceEntry svcs ++ map mkExternalEntry exts;
in
{
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
