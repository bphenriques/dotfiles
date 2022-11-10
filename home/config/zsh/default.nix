{ config, lib, pkgs, ... }:

with lib;
{
  home.packages = with pkgs; [
    vivid       # LS_COLORS generator because I refuse to use the syntax >.<
    thefuck     # Fix some commands
  ];

  home = {
    sessionVariables = {
      TERM    = "screen-256color";              # Ensure term is set with the right color

      # Set locale and UTF-8
      LANG    = "en_US.UTF-8";
      LC_ALL  = "en_US.UTF-8";

      # Default editors and settings
      EDITOR  = "emacsclient";
      VISUAL  = "$EDITOR";
      PAGER   = "less -iMR";

      # Colors
      CLICOLOR  = 1;                            # Enable ls colors in MacOS.
      LS_COLORS ="$(vivid generate snazzy)";    # Generates the color palette.

      WORKSPACE = "$HOME/workspace";            # Default directory for repositories
    };

    shellAliases = {
      # Files
      ls    = "ls --color=auto";
      la    = "ls -la";
      mkdir = "mkdir -pv";

      # Text Processor
      e           = "emacsclient";
      grep        = "grep --color";
    };
  };

  programs.direnv = {
    enable                  = true; # Automatically load .envrc or .env.
    nix-direnv.enable       = true; # Faster direnv for nix environments.
    extra.disableLogging    = true; # Disable verbose messages when entering a directory.
  };

  modules = {
    powerlevel10k = {
      enable                  = true;
      personalZshIntegration  = true;
      configuration           = ./powerlevel10k.theme.zsh;
    };

    thefuck = {
      enable                  = true;
      personalZshIntegration  = true;
    };

    zsh = {
      enable = true;
      envExtra = concatStringsSep "\n" [
        # MacOS: Homebrew
        (optionalString (pkgs.stdenv.system == "aarch64-darwin") ''eval "$(/opt/homebrew/bin/brew shellenv)"'')

        # Local session variables
        ''[ -s "$HOME"/.zshenv.local ] && source "$HOME"/.zshenv.local''
      ];

      options = builtins.readFile ./options.zsh;
      plugins = mkAfter [
        {
          name = "zsh-autosuggestions";
          src = pkgs.zsh-autosuggestions;
          file = "share/zsh-autosuggestions/zsh-autosuggestions.zsh";
          sourceExtra = ''export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#3e4551"'';
        }
        {
          name = "zsh-fast-syntax-highlighting";
          src = pkgs.zsh-fast-syntax-highlighting;
          file = "share/zsh/site-functions/fast-syntax-highlighting.plugin.zsh";
          sourceTiming = "last";
        }
      ];

      functions = [
        ./functions/dotfiles.zsh
        ./functions/load-env.zsh
      ];

      # Load any local zshrc scripts if present.
      initExtraAfterCompInit = ''[ -s "$HOME"/.zshrc.local ] && source "$HOME"/.zshrc.local'';
    };
  };
}
