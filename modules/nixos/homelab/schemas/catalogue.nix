{ lib, ... }:
{
  options.integrations.catalogue = {
    enable = lib.mkEnableOption "service catalogue entry" // {
      default = true;
    };
  };
}
