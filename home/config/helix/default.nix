{ config, lib, pkgs, ... }:

let
  # The path needs to be absolute: https://github.com/nix-community/home-manager/pull/1455#issuecomment-681041818
  helixCfg = "${config.home.homeDirectory}/.dotfiles/home/config/helix/config.toml";
  helixLanguages = "${config.home.homeDirectory}/.dotfiles/home/config/helix/languages.toml";
in
{
  programs.helix = {
    enable = true;

    # All the LSP. Check with hx --health
    extraPackages = with pkgs; [
      marksman                                # LSP for Markdown
      nodePackages.bash-language-server       # LSP for Bash
      nodePackages.yaml-language-server       # LSP for YAML
      nodePackages.vscode-json-languageserver # LSP for JSON
      texlab                                  # LSP for LaTeX
      nil                                     # LSP for Nix
      terraform-ls                            # LSP for Terraform
    ];
  };

  # Out of Nix Store file which allows me to try things out without having to recompile the project.
  xdg.configFile = {
    "helix/config.toml".source = config.lib.file.mkOutOfStoreSymlink "${helixCfg}";
    "helix/languages.toml".source = config.lib.file.mkOutOfStoreSymlink "${helixLanguages}";
  };
}
