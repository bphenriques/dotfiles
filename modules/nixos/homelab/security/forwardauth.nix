# Forward-auth contract (neutral): the endpoint an ingress provider points its forward-auth
# middleware at, supplied by the active forward-auth provider (e.g. tinyauth). Per-service opt-in is
# `services.<name>.forwardAuth.enable` (schemas/ingress.nix). Active when `url` is non-empty.
{ lib, ... }:
{
  options.custom.homelab.forwardAuth = {
    url = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "Base URL of the forward-auth endpoint; set by the active forward-auth provider, consumed by the ingress provider.";
    };
    path = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = ''
        Verify path appended to `url` for the ingress provider's forward-auth middleware
        (e.g. /api/auth/traefik). Set by the active provider for the ingress it targets; override
        when pairing the provider with a different ingress.
      '';
    };
  };
}
