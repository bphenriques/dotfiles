{ name, lib, ... }:
{
  options.backup.outputDir = lib.mkOption {
    type = lib.types.str;
    default = "/var/lib/homelab-backup/src/extras/${name}";
    readOnly = true;
    description = "Directory where the hook writes its output. Available as OUTPUT_DIR in the hook environment.";
  };
}
