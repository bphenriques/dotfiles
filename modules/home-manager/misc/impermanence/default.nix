{ lib, config, inputs, ... }:

with lib;
let
  cfg = config.custom.impermanence;
in
{
  options.custom.impermanence = {
    enable = lib.mkEnableOption "Whether to enable home-manager impermanence. Only usable in Nixos";
    configLocation = lib.mkOption {
      type = with lib.types; str;
      description = "Location of the users's configuration persist directory";
    };

    cacheLocation = lib.mkOption {
      type = with lib.types; str;
      description = "Location of the users's configuration persist directory";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
         assertion = pkgs.stdenv.isLinux;
         message = "custom.impermanence is only available in NixOS.";
      }
    ];
  };
}
