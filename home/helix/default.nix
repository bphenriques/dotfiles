{ config, pkgs, ... }:

# TODO: Desktop entry? https://github.com/balsoft/nixos-config/blob/73cc2c3a8bb62a9c3980a16ae70b2e97af6e1abd/profiles/applications/helix.nix#L8

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
      ltex-ls                                 # LSP for grammar/spell check
    ];
  };

  # Out of Nix Store file which allows me to try things out without having to recompile the project.
  xdg.configFile = {
    "helix/config.toml".source = config.lib.file.mkOutOfStoreSymlink "${helixCfg}";
    "helix/languages.toml".source = config.lib.file.mkOutOfStoreSymlink "${helixLanguages}";
  };
}
