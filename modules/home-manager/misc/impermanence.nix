{ lib, config, ... }:

with lib;
{
  options.custom.impermanence = {
    configLocation = lib.mkOption {
      type = with lib.types; str;
      description = "Location of the users's configuration persist directory";
    };

    cacheLocation = lib.mkOption {
      type = with lib.types; str;
      description = "Location of the users's configuration persist directory";
    };
  };
}
