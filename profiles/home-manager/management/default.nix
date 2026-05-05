{ pkgs, lib, osConfig, self, ... }:
let
  financeDir = osConfig.custom.homelab.paths.users.bphenriques.finance;
in
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [
    pkgs.hledger

    # Custom wrapper
    self.packages.fin

    # UI
    pkgs.hledger-ui
    pkgs.puffin
  ];
  home.sessionVariables = {
    FIN_DIR = financeDir;
  };
}
