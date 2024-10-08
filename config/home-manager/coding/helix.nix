{ config, pkgs, lib, ... }:

{
  # Check more ideas in https://github.com/maximbaz/dotfiles/blob/8e487136d8eab0568b8ee3d44d9973e694f332ed/modules/common/helix.nix
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

    languages = {
      languages = [
        { name = "rust";    auto-format = false; }
        { name = "scala";   auto-format = false; }
        { name = "markdown"; language-servers = [ "marksman" "ltex-ls" ]; }
      ];

      language-server = {
        ltex-ls = {
          command = "ltex-ls";
          config = { ltex = { language = "en-GB"; }; }; # https://valentjn.github.io/ltex/settings.html
        };
      };
    };

    settings = {
      theme = "onedark";
      editor = {
        bufferline = "multiple";
        color-modes = true;
        insert-final-newline = true;
        line-number = "absolute";
        rulers = [ 120 ];
        text-width = 120;
        auto-format	= false;

        cursor-shape = {
          insert = "bar";
          normal = "block";
          select = "underline";
        };

        lsp = {
          enable = true;
          display-inlay-hints = true;
          display-messages = true;
        };

        auto-save = {
          focus-lost = true;
          after-delay.enable = true;
        };

        statusline = {
          right = [ "diagnostics" "selections" "register" "position" "file-encoding" "version-control" ];

          #right = [ "diagnostics" "selections" "register" "file-type" "file-line-ending" "position" ];
          #mode.normal = "";
          #mode.insert = "I";
          #mode.select = "S";
        };

        indent-guides = {
          render = true;
          character = "┊";
        };
      };

      keys = {
        normal = {
          C-j = "save_selection";
          C-s = ":w";
          esc = [ "collapse_selection" "keep_primary_selection" ];

          space.q = ":q";
          space.space = "file_picker";
          space.w = ":w";
        };
      };
    };
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

  home.sessionVariables.EDITOR  = "${pkgs.helix}/bin/hx";
  custom.xdgDefaultApps.text = lib.mkBefore [ "helix.desktop" ];
}
