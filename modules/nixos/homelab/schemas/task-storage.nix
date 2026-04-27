{ lib, homelabCfg, ... }:
{
  options.storage.smb = lib.mkOption {
    type = lib.types.listOf (lib.types.enum (lib.attrNames homelabCfg.smb.mounts));
    default = [ ];
    description = "Named homelab SMB mounts this task requires.";
  };
}
