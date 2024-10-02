{ pkgs, config, lib, ... }:

with lib;
let
  cfg = config.custom.lutris;
in
{
  options.custom.lutris = with types; {
    enable = mkEnableOption "lutris";
  };

  config = mkIf cfg.enable {
    home.packages = [
      (pkgs.lutris.override {
        extraPkgs = pkgs: [
          pkgs.kdialog   # Required otherwise installation might not work. FIXME: may only make sense for KDE
          pkgs.protobuf  # Required for battlenet.
        ];
      })
    ];
  };
}
