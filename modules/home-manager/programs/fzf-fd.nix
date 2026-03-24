{ config, lib, self, ... }:

let
  cfg = config.custom.programs.fzf-fd;
in {
  options.custom.programs.fzf-fd = {
    enable = lib.mkEnableOption "fzf-fd";

    package = lib.mkOption {
      type = lib.types.package;
      default = self.pkgs.fzf-fd;
      description = "package to install.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];
  };
}
