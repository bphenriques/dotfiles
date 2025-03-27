{ config, pkgs, lib, self, ... }:
{
  stylix.targets.helix.enable = true;
  programs.helix = {
    enable = true;
    defaultEditor = true;
    package = pkgs.helix-git;

    # Language Server Protocols. Check with hx --health
    extraPackages = [
      pkgs.marksman                                # LSP for Markdown
      pkgs.nodePackages.bash-language-server       # LSP for Bash
      pkgs.nodePackages.yaml-language-server       # LSP for YAML
      pkgs.nodePackages.vscode-json-languageserver # LSP for JSON
      pkgs.vscode-langservers-extracted            # LSP for HTML/CSS/JSON/ESLint
      pkgs.typescript-language-server              # LSP for Typescript
      pkgs.docker-compose-language-service         # LSP for docker-compose
      pkgs.dockerfile-language-server-nodejs       # LSP for docker
      pkgs.texlab                                  # LSP for LaTeX
      pkgs.nil                                     # LSP for Nix
      pkgs.terraform-ls                            # LSP for Terraform
      pkgs.ltex-ls                                 # LSP for grammar/spell check
    ];

    languages = {
      languages = [
        { name = "rust";    auto-format = false; }
        { name = "scala";   auto-format = false; }
        { name = "markdown"; language-servers = [ "marksman" "ltex-ls" ]; }
        {
          name = "nix";
          auto-pairs = {
            "=" = ";";
            "(" = ")";
            "{" = "}";
            "[" = "]";
            "'" = "'";
            "\"" = "\"";
            "`" = "`";
          };
        }
      ];

      language-server = {
        ltex-ls = {
          command = "ltex-ls";
          config = { ltex = { language = "en-GB"; }; };
        };
      };
    };

    settings = {
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
          left = [ "mode" "file-name" "spinner" "read-only-indicator" "file-modification-indicator" ];
          right = [ "diagnostics" "selections" "register" "position" "file-encoding" "version-control" ];
        };

        indent-guides = {
          render = true;
          character = "┊";
          skip-levels = 1;
        };

        # New recommdended settings
        end-of-line-diagnostics = "hint";
        inline-diagnostics.cursor-line = "warning"; # show warnings and errors on the cursorline inline
      };

      keys = {
        normal = {
          C-s = ":write";
          esc = [ "collapse_selection" "keep_primary_selection" ];
          S-left = "jump_backward";
          S-right = "jump_forward";
          # tab = "collapse_selection";

          space = {
            q = ":write-quit-all";
            Q = ":quit!";
            space = "file_picker";
            w = ":write";
         };

          # Smart Indent
          tab = "move_parent_node_end";
          S-tab = "move_parent_node_start";
        };

        insert = {
          # Smart indent
          S-tab = "move_parent_node_start";
        };

        select = {
          # Smart indent
          tab = "extend_parent_node_end";
          S-tab = "extend_parent_node_start";
        };
      };
    };
  };

  custom.xdgDefaultApps.text = lib.mkBefore [ "Helix.desktop" ];
}
