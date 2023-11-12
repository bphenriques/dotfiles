{ config, lib, pkgs, ... }:

with lib;
{
  home.packages = with pkgs; [
    vivid       # LS_COLORS generator because I refuse to use the syntax >.<
  ];

  home = {
    sessionVariables = {
      TERM    = "screen-256color";              # Ensure term is set with the right color

      # Set locale and UTF-8
      LANG    = "en_US.UTF-8";
      LC_ALL  = "en_US.UTF-8";

      # Default editors and settings
      EDITOR  = "hx";
      VISUAL  = "$EDITOR";
      PAGER   = "less -iMR";

      # Colors
      CLICOLOR  = 1;                            # Enable ls colors in MacOS.
      LS_COLORS ="$(vivid generate snazzy)";    # Generates the color palette.

      WORKSPACE = "$HOME/workspace";            # Default directory for repositories
    };

    shellAliases = {
      # Default colorizatio
      diff = "diff --color=auto";
      grep = "grep --color=auto";
      egrep = "egrep --color=auto";
      fgrep = "fgrep --color=auto";
      ls = "ls --color=auto";

      # The usual aliases
      l = "ls -alh";
      ll = "ls -l";

      # Quality of life
      mkdir = "mkdir -pv";
      ".."  = "cd ..";
      "..."  = "cd ../..";

      # Text Processor
      e           = "$EDITOR";
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

    zsh = {
      enable = true;
      envExtra = concatStringsSep "\n" [
        # MacOS: Homebrew
        (optionalString (pkgs.stdenv.system == "aarch64-darwin") ''eval "$(/opt/homebrew/bin/brew shellenv)"'')

        # Local session variables
        ''[ -s "$HOME"/.zshenv.local ] && source "$HOME"/.zshenv.local''
      ];

      # History - http://zsh.sourceforge.net/Doc/Release/Options.html
      options = [
        "HIST_FCNTL_LOCK"                  # Safer and faster locking in newer OS.
        "APPEND_HISTORY"                   # Appends history to history file on exit.
        "INC_APPEND_HISTORY"               # Write to the history file immediately, not when the shell exits.
        "SHARE_HISTORY"                    # Share history between all sessions.
        "HIST_EXPIRE_DUPS_FIRST"           # Expire a duplicate event first when trimming history.
        "HIST_IGNORE_DUPS"                 # Do not record an event that was just recorded again.
        "HIST_IGNORE_ALL_DUPS"             # Delete an old recorded event if a new event is a duplicate.
        "HIST_FIND_NO_DUPS"                # Do not display a previously found event.
        "HIST_IGNORE_SPACE"                # Do not record an event starting with a space.
        "HIST_SAVE_NO_DUPS"                # Do not write a duplicate event to the history file.
        "HIST_REDUCE_BLANKS"               # Minimize unnecessary whitespace
        "HIST_VERIFY"                      # Do not execute immediately upon history expansion.
        "HIST_BEEP"                        # Beep when accessing non-existent history.
      ];

      keyBindingsMode = "emacs";           # Explicitly set emacs as my keyboard mapping despite having vim as $EDITOR.

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
