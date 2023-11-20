{ config, home-manager, lib, ... }:

with lib;
with types;

let
  cfg = config.user;
in
{
  # Setup an alias from home-manager's root configuration to just "home".
  # TBH: Implicits are bad. Workaround to have modular modules that work regardless of the user.
  imports = [
    (mkAliasOptionModule ["home"] ["home-manager" "users" config.user.name])
  ];

  options.user = {
    name = mkOption { type = str; };
    extraGroups = mkOption { type = listOf str; default = []; };
    musicDir = mkOption { type = str; };
    romsDir = mkOption { type = str; };
    shareDir = mkOption { type = str; };
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
