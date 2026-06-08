# Ingress-level service-schema contributions: the (neutral) forward-auth gate and the
# Traefik-specific middleware escape hatch. Kept out of the transport-neutral base module so the
# registry stays a contract any ingress implementation could consume.
{ lib, ... }:
{
  options = {
    forwardAuth.enable = lib.mkEnableOption "ingress-level access control via the forward-auth gateway";

    traefik.middlewares = lib.mkOption {
      type = lib.types.attrsOf (lib.types.attrsOf lib.types.unspecified);
      default = { };
      description = "Extra Traefik middleware definitions to attach to this service's router";
    };
  };
}
