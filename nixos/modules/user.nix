{ config, lib, pkgs, ... }:

with lib;
with types;

let
  cfg = config.user;
in
{
  options.user = {
    name = mkOption { type = str; };
    extraGroups = mkOption { type = listOf str; default = []; };
  };

  config = {
    users.users.${cfg.name} = {
      description = "The primary user";
      isNormalUser = true;
      extraGroups = ["wheel"] ++ cfg.extraGroups;
    };
  };
}
