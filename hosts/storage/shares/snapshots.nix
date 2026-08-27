{ config, lib, ... }:
let
  datasets = lib.concatMap (
    share:
    lib.optional share.snapshots "tank/${share.dataset}"
    ++ map (child: "tank/${share.dataset}/${child}") share.childDatasets
  ) (lib.attrValues config.custom.storage.shares);
in
{
  services.sanoid = {
    enable = true;
    interval = "*-*-* 00/2:00:00";
    templates.storage = {
      hourly = 60;
      daily = 60;
      monthly = 0;
      yearly = 0;
      autosnap = true;
      autoprune = true;
    };
    datasets = lib.genAttrs datasets (_: { useTemplate = [ "storage" ]; });
  };
}
