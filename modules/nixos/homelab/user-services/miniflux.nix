{ lib, ... }:
{
  config.custom.homelab._userOptionExtensions = [
    (_: {
      options.services.miniflux = {
        enable = lib.mkEnableOption "Miniflux settings for this user";
        settings = lib.mkOption {
          type = lib.types.attrs;
          default = { };
          description = ''
            Miniflux user settings applied via API (theme, display_mode, stylesheet, etc).
            WARNING: Do not put secrets here - this data is stored in the world-readable Nix store.
            See https://miniflux.app/docs/api.html#update-user for available fields.
          '';
          example = { theme = "dark_serif"; display_mode = "fullscreen"; };
        };
      };
    })
  ];
}
