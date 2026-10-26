{ lib, pkgs, config, self, ... }:
let
  shellPlugin = pkg: {
    inherit (pkg) name;
    src = pkg;
    file = "share/zsh/plugins/${pkg.name}/${pkg.name}.plugin.zsh";
  };
in
{
  programs.zsh = {
    enable = true;
    dotDir = "${config.xdg.configHome}/zsh";
    defaultKeymap = "emacs";
    autocd = true;
    setOptions = [
      "INTERACTIVE_COMMENTS" "HIST_REDUCE_BLANKS" "HIST_VERIFY" "INC_APPEND_HISTORY"
      "NO_FLOW_CONTROL" "NO_BEEP"
      "AUTO_PUSHD" "PUSHD_IGNORE_DUPS" "PUSHD_SILENT"
      "COMPLETE_IN_WORD" "ALWAYS_TO_END"
    ];

    history = {
      path = "${config.xdg.stateHome}/zsh/history";
      size = 100000;
      ignoreAllDups = true;
      ignorePatterns = [ "rm *" "sudo rm *" "git reset --hard*" "git clean *" ];
      share = false;
      extended = true;
    };

    autosuggestion = {
      enable = true;
      strategy = [ "history" "completion" ];
    };
    fastSyntaxHighlighting.enable = true;
    historySubstringSearch = {
      enable = true;
      searchUpKey = [ "^[[A" "^[OA" ];
      searchDownKey = [ "^[[B" "^[OB" ];
    };

    plugins = [
      { name = "fzf-tab"; src = pkgs.zsh-fzf-tab; file = "share/fzf-tab/fzf-tab.plugin.zsh"; }
      { name = "autopair"; src = pkgs.zsh-autopair; file = "share/zsh/zsh-autopair/autopair.zsh"; }
      (shellPlugin self.packages.fzf-rg)
    ] ++ lib.optional config.my.programs.project.enable (shellPlugin config.my.programs.project.package)
      ++ lib.optional config.my.dotfiles.enable (shellPlugin config.my.dotfiles.package);

    initContent = ''
      fpath+=(${pkgs.pure-prompt}/share/zsh/site-functions)
      zstyle :prompt:pure:environment:virtualenv show no
      zstyle :prompt:pure:prompt:success color green
      prompt_newline=' '   # Single line; pure keeps a non-empty value.
      autoload -Uz prompt_pure_setup && prompt_pure_setup

      zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
      zstyle ':completion:*' menu no
      zstyle ':completion:*' rehash true
      zstyle ':completion:*:git-checkout:*' sort false
      zstyle ':fzf-tab:complete:*' fzf-preview '${lib.getExe self.packages.preview} $realpath'

      autoload -Uz select-word-style bracketed-paste-url-magic edit-command-line
      select-word-style bash
      zle -N bracketed-paste bracketed-paste-url-magic
      zle -N edit-command-line

      __yazi-widget() {
        BUFFER=${config.programs.yazi.shellWrapperName}
        zle accept-line
      }
      zle -N __yazi-widget

      bindkey '^[/' __frg-widget        # Find text (fzf-rg)
      bindkey '^[p' __project-widget    # Switch project
      bindkey '^[ ' __yazi-widget       # Browse files
      bindkey '^[e' edit-command-line
      bindkey '^[[1;3C' forward-word    # Alt+Right
      bindkey '^[[1;3D' backward-word   # Alt+Left
      bindkey '^[[1;5C' forward-word    # Ctrl+Right
      bindkey '^[[1;5D' backward-word   # Ctrl+Left

      [[ -f $ZDOTDIR/local.zsh ]] && source $ZDOTDIR/local.zsh
    '';
  };
}
