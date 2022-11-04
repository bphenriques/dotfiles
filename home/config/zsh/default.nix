{ config, lib, pkgs, ... }:

with lib;
{
  home.packages = with pkgs; [
    vivid       # LS_COLORS generator because I refuse to use the syntax >.<

    # Utility
    ripgrep     # Alternative to grep.
    bat         # Better file preview.
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
      e           = "$EDITOR";
      grep        = "grep --color";
    };
  };

  programs.direnv = {
    enable                          = true; # Automatically load .envrc or .env.
    nix-direnv.enable               = true; # Faster direnv for nix environments.
    extra = {
      enablePowerlevel10kFastPrompt = true; # Setup fast-prompt
      disableLogging = true;                # Disable verbose messages when entering a directory.
    };
  };

  modules = {
    powerlevel10k = {
      enable                = true;
      enableZshIntegration  = true;
      configuration         = ./powerlevel10k.theme.zsh;
      fastPrompt.enable     = true;
    };

    thefuck = {
      enable                = true;
      enableZshIntegration  = true;
    };

    zsh = {
      enable = true;
      envExtra = concatStringsSep "\n" [
        # MacOS: Homebrew
        (optionalString (pkgs.stdenv.system == "aarch64-darwin") ''eval "$(/opt/homebrew/bin/brew shellenv)"'')

        # Local session variables
        ''[ -s "$HOME"/.zshenv.local ] && source "$HOME"/.zshenv.local''
      ];

      initExtraBeforePlugins = (builtins.readFile ./config.zsh);

      plugins = {
        enableFastSyntaxHighlighting = true;
        list = [
          {
            name = "zsh-autosuggestions";
            src = pkgs.zsh-autosuggestions;
            file = "share/zsh-autosuggestions/zsh-autosuggestions.zsh";
            afterSource = ''export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#3e4551"'';
          }
        ];
      };

      functions = [
        ./functions/dotfiles.zsh
        ./functions/load-env.zsh
      ];

      initExtraBeforeCompInit = ''
        zstyle ':completion:*' menu select=2        # Makes sure that tab-completion is available iff number_items > 2
      '';

      initExtraAfterCompInit = ''
        # Load any local zshrc scripts if present.
        [ -s "$HOME"/.zshrc.local ] && source "$HOME"/.zshrc.local
      '';
    };
  };
}
