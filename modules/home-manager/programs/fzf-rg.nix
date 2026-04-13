{ config, lib, self, ... }:

let
  cfg = config.custom.programs.fzf-rg;
in {
  options.custom.programs.fzf-rg = {
    enable = lib.mkEnableOption "fzf-rg";

    package = lib.mkOption {
      type = lib.types.package;
      default = self.packages.fzf-rg;
      description = "package to install.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];
  };
}
