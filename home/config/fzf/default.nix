{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ fzf ]; # Not using `enable: true` as I manage by my own config.

  xdg.configFile."zsh/modules/fzf.zsh".text = ''
    . ${pkgs.fzf}/share/fzf/completion.zsh
    . ${pkgs.fzf}/share/fzf/key-bindings.zsh

    export FZF_DEFAULT_OPTS="--bind='ctrl-p:toggle-preview' --bind='ctrl-f:jump' --marker='* ' --pointer='▶'"
    export FZF_DEFAULT_COMMAND="fd --type file --hidden"
    export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
  '';
}
