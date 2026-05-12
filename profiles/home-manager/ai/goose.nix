{ lib, pkgs, self, ... }:
{
  custom.programs.goose = {
    enable = true;
    servers = {
      # vault = {  # TODO: enable when packages/ai/vault-mcp is built
      #   enable = true;
      #   name = "Vault";
      #   cmd = lib.getExe self.packages.vault-mcp;
      # };
      fetch = {
        enable = true;
        name = "Fetch";
        cmd = lib.getExe pkgs.mcp-server-fetch;
      };
    };
  };
}
