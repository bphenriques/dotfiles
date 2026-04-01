{ lib, ... }:
{
  options.custom.homelab.locale = {
    timezone = lib.mkOption {
      type = lib.types.str;
      description = "Timezone for services (e.g. 'Europe/Lisbon')";
    };

    language = lib.mkOption {
      type = lib.types.str;
      description = "BCP 47 locale identifier for regional formatting (e.g. 'pt-PT')";
    };

    currency = lib.mkOption {
      type = lib.types.str;
      description = "ISO 4217 currency code (e.g. 'EUR')";
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
