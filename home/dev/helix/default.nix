{ config, pkgs, ... }:
{
  /*
  home.packages = [
    (pkgs.makeDesktopItem {
      name = "helix";
      desktopName = "Helix editor";
      terminal = true;
      categories = [ "Utility" "TextEditor" "Development" "IDE" ];
      mimeTypes = [
        "inode/directory"
        "text/english"
        "text/plain"
        "text/x-makefile"
        "text/x-c++hdr"
        "text/x-c++src"
        "text/x-chdr"
        "text/x-csrc"
        "text/x-java"
        "text/x-moc"
        "text/x-pascal"
        "text/x-tcl"
        "text/x-tex"
        "application/x-shellscript"
        "application/json"
        "application/xml"
        "text/xml"
        "text/x-c"
        "text/x-c++"
      ];
      exec = "${pkgs.helix}/bin/hx %F";
      icon = "helix";
    })
  ];
  */

  programs.helix = {
    enable = true;

    # Language Server Protocols. Check with hx --health
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

  # TODO: https://codeberg.org/adamcstephens/dotfiles/src/branch/main/apps/helix/default.nix
  xdg.configFile = {
    "helix/config.toml".source = config.lib.file.mkOutOfStoreSymlink "${config.custom.dotfiles.directory}/home/helix/config.toml";
    "helix/languages.toml".source = config.lib.file.mkOutOfStoreSymlink "${config.custom.dotfiles.directory}/home/helix/languages.toml";
  };
}
