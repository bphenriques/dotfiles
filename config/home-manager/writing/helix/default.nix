{ config, pkgs, ... }:

# TODO: https://github.com/maximbaz/dotfiles/blob/8e487136d8eab0568b8ee3d44d9973e694f332ed/modules/common/helix.nix
{
  programs.helix = {
    enable = true;

    # Language Server Protocols. Check with hx --health
    extraPackages = with pkgs; [
      marksman                                # LSP for Markdown
      nodePackages.bash-language-server       # LSP for Bash
      nodePackages.yaml-language-server       # LSP for YAML
      nodePackages.vscode-json-languageserver # LSP for JSON
      docker-compose-language-service         # LSP for docker-compose
      dockerfile-language-server-nodejs       # LSP for docker
      texlab                                  # LSP for LaTeX
      nil                                     # LSP for Nix
      terraform-ls                            # LSP for Terraform
      ltex-ls                                 # LSP for grammar/spell check
    ];
  };

  xdg.configFile = {
    "helix/config.toml".source = config.lib.file.mkOutOfStoreSymlink "${config.custom.dotfiles.directory}/home-manager/dev/helix/config.toml";
    "helix/languages.toml".source = config.lib.file.mkOutOfStoreSymlink "${config.custom.dotfiles.directory}/home-manager/dev/helix/languages.toml";
  };

  home.packages = [
    (pkgs.makeDesktopItem {
      name = "helix";
      desktopName = "Helix editor";
      terminal = true;
      categories = [ "Utility" "TextEditor" "Development" "IDE" ];
      mimeTypes = [
        "text/plain"
        "application/json"
        "application/xml"

        "application/x-shellscript"
        "text/x-makefile"
        "text/x-tex"
        "text/x-java"
        "text/x-chdr"
        "text/x-csrc"
        "text/x-c++hdr"
        "text/x-c++src"
        "text/xml"
      ];
      exec = "${pkgs.helix}/bin/hx %F";
      icon = "helix";
    })
  ];
}
