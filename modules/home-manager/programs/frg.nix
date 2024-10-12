{ config, lib, pkgs, self, ... }:

with lib;

let
  cfg = config.custom.programs.frg;
in {
  options.custom.programs.frg = {
    enable = mkEnableOption "frg";

    # FIXME: this is not portable
    package = mkOption {
      type = types.package;
      default = self.pkgs.frg;
      description = ''
        frg package to install.
      '';
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
  };
}