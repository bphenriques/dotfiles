{ pkgs, lib, osConfig, self, ... }:
let
  financeDir = osConfig.custom.homelab.paths.users.bphenriques.finance;
in
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [ self.packages.fin self.packages.fin2 self.packages.fin3 ];
  home.sessionVariables = {
    LEDGER_FILE = "${financeDir}/current.journal";
    FIN_DIR = financeDir;
  };
}
