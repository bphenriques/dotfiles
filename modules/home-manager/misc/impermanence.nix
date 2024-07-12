{ lib, config, ... }:

with lib;
let
  customDirOptions = {
    directory = mkOption { type = str; };
    user = mkOption { type = str; };
    group = mkOption { type = str; };
    mode = mkOption { type = str; };
  };
  persistStorageConfig = types.submodule {
    options = {
      location = lib.mkOption {
        type = with lib.types; str;
        description = "Location of the persist directory";
      };
      directories = lib.mkOption {
        type = with lib.types; listOf str;
        default = [ ];
        description = "Home directories to persist";
      };
      files = lib.mkOption {
        type = with lib.types; listOf str;
        default = [ ];
        description = "Home files to persist";
      };
    };
  };
in
{
  options.custom.impermanence = {
    config = mkOption {
      type = persistStorageConfig;
      description = "Config files or directories not managed through Nix.";
    };

    cache = mkOption {
      type = persistStorageConfig;
      description = "Cache files or directories that are safe to delete but we opt to persist";
    };
  };
}
