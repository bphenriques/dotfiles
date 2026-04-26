{ lib, ... }:
{
  options.services.oidc.enable = lib.mkEnableOption "OIDC account for this user" // {
    default = true;
  };
}
