# Traefik ingress contribution to the service schema.
# Keeps Traefik-specific options out of the transport-neutral base service module,
# so the registry stays a contract any ingress implementation could consume.
{ lib, ... }:
{
  options = {
    # Ingress-level authentication (Traefik forwardAuth, mutually exclusive with OIDC)
    forwardAuth.enable = lib.mkEnableOption "ingress-level access control via Traefik forwardAuth";

    traefik.middlewares = lib.mkOption {
      type = lib.types.attrsOf (lib.types.attrsOf lib.types.unspecified);
      default = { };
      description = "Extra Traefik middleware definitions to attach to this service's router";
    };
  };
}
