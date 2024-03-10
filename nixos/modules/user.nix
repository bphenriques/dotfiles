{ config, home-manager, lib, ... }:

with lib;
with types;

let
  cfg = config.user;
in
{
  # Setup an alias from home-manager's root configuration to just "home".
  # TBH: This is a bad pattern. This is a workaround in-order to have modules that do not need to depend on the current user.
  imports = [
    (mkAliasOptionModule ["home"] ["home-manager" "users" config.user.name])
  ];

  options.user = {
    name = mkOption { type = str; };
    extraGroups = mkOption { type = listOf str; default = []; };
    musicDir = mkOption { type = str; };
    romsDir = mkOption { type = str; };
    protonDefaultPrefixDir = mkOption { type = str; };
  };

  config = {
    users.users.${cfg.name} = {
      description = "The primary user";
      isNormalUser = true;
      home = "/home/${cfg.name}";
      extraGroups = ["wheel"] ++ cfg.extraGroups;
    };
  };
}
