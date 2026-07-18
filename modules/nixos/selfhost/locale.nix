{ lib, ... }:
{
  options.custom.locale = {
    timezone = lib.mkOption {
      type = lib.types.str;
      description = "Timezone for services (e.g. 'Europe/Lisbon')";
    };

    latitude = lib.mkOption {
      type = lib.types.float;
      description = "Geographic latitude (e.g. 38.736946 for Lisbon)";
    };

    longitude = lib.mkOption {
      type = lib.types.float;
      description = "Geographic longitude (e.g. -9.142685 for Lisbon)";
    };
  };
}
