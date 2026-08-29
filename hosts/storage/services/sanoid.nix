{ config, lib, ... }:
let
  datasets = lib.concatMap (
    share:
    lib.optional share.snapshots share.dataset
    ++ map (child: "${share.dataset}/${child}") share.childDatasets
  ) (lib.attrValues config.custom.storage.shares);
in
{
  services.sanoid = {
    enable = true;
    interval = "*-*-* 00/2:00:00";
    # The undo button, not the backup: same pool, same failure domain, so the long tail lives in B2.
    templates.storage = {
      hourly = 60;
      daily = 60;
      monthly = 3;
      yearly = 0;
      autosnap = true;
      autoprune = true;
    };
    datasets = lib.genAttrs datasets (_: { useTemplate = [ "storage" ]; });
  };
}
