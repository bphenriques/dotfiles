{ pkgs, lib, osConfig, self, ... }:
let
  financeDir = osConfig.custom.homelab.paths.users.bphenriques.finance;
in
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [ self.packages.fin ];
  home.sessionVariables = {
    LEDGER_FILE = "${financeDir}/current.journal";
    FIN_DIR = financeDir;
  };
}
