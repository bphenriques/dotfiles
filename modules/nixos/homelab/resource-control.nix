{ lib, config, ... }:
let
  inherit (config.custom) homelab;
  rcCfg = homelab.resourceControl;
  sliceNames = lib.attrNames rcCfg.slices;

  serviceExtension = _: {
    options.resourceControl = {
      slice = lib.mkOption {
        type = lib.types.nullOr (lib.types.enum sliceNames);
        default = null;
        description = "Shared systemd slice for this service's declared systemdServices.";
      };

      systemdServices = lib.mkOption {
        type = lib.types.coercedTo lib.types.str (s: [ s ]) (lib.types.listOf lib.types.str);
        default = [ ];
        description = "Systemd service names assigned to the chosen slice. Required when slice is set.";
      };
    };
  };

  collectUnits = sliceName:
    let
      sliceCfg = rcCfg.slices.${sliceName};

      fromServices = lib.concatMap (svc:
        lib.optionals (svc.resourceControl.slice == sliceName) svc.resourceControl.systemdServices
      ) (lib.attrValues homelab.services);
    in
      lib.unique (fromServices ++ sliceCfg.extraSystemdServices);

  collectedUnits = lib.mapAttrs (name: _: collectUnits name) rcCfg.slices;

  mkSliceOverrides = sliceName: units:
    lib.listToAttrs (map (name: lib.nameValuePair name {
      serviceConfig.Slice = lib.mkForce "${sliceName}.slice";
    }) units);

  servicesMissingUnits = lib.attrNames (lib.filterAttrs (_: svc:
    svc.resourceControl.slice != null && svc.resourceControl.systemdServices == [ ]
  ) homelab.services);
in
{
  options.custom.homelab.resourceControl = {
    slices = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule {
        options = {
          extraSystemdServices = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ ];
            description = "Non-registry systemd services to place in this slice.";
          };

          sliceConfig = lib.mkOption {
            type = lib.types.attrsOf lib.types.unspecified;
            default = { };
            description = "Systemd slice resource limits (e.g. CPUQuota, MemoryHigh, CPUWeight).";
          };
        };
      });
      default = { };
      description = "Named systemd slices for aggregate resource control.";
    };

  };

  config = lib.mkMerge [
    { custom.homelab._serviceOptionExtensions = [ serviceExtension ]; }

    (lib.mkIf homelab.enable {
      assertions = [{
        assertion = servicesMissingUnits == [ ];
        message = "Services with resourceControl.slice must declare resourceControl.systemdServices: ${toString servicesMissingUnits}";
      }];

      systemd.slices = lib.mapAttrs (sliceName: _units: {
        inherit (rcCfg.slices.${sliceName}) sliceConfig;
      }) (lib.filterAttrs (_: units: units != [ ]) collectedUnits);

      systemd.services = lib.mkMerge (
        lib.mapAttrsToList (sliceName: units:
          lib.mkIf (units != [ ]) (mkSliceOverrides sliceName units)
        ) collectedUnits
      );
    })
  ];
}
