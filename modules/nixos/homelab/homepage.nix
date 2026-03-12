{ lib, config, ... }:
let
  cfg = config.custom.homelab;

  homepageServices = lib.filter
    (s: s.integrations.homepage != null && s.integrations.homepage.enable)
    (lib.attrValues cfg.services);

  mkServiceEntry = service: {
    "${service.name}" = {
      href = service.publicUrl;
      description = service.integrations.homepage.description;
      icon = service.integrations.homepage.icon;
    } // lib.optionalAttrs service.integrations.homepage.siteMonitor {
      siteMonitor = service.publicUrl;
    };
  };

  servicesByCategory = lib.groupBy (s: s.integrations.homepage.category) homepageServices;
in
{
  options.custom.homelab.homepage = {
    generatedServices = lib.mkOption {
      type = lib.types.attrsOf (lib.types.listOf lib.types.anything);
      default = lib.mapAttrs (_: svcs: map mkServiceEntry svcs) servicesByCategory;
      readOnly = true;
      description = "Auto-generated homepage services grouped by category (read-only)";
    };
  };
}
