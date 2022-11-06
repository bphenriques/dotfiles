{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    tmux
    tmuxPlugins.yank
  ];

  home.shellAliases = {
    reload    = "tmux respawn-pane -k";
    killtmux  = "tmux kill-server";
  };

  xdg.configFile = {
    "tmux/tmux.conf".source        = ./tmux.conf;
    "tmux/tmux.theme.conf".source  = ./tmux.theme.conf;
    "tmux/tmux.plugins.conf".text  = ''
      run-shell ${pkgs.tmuxPlugins.yank}/share/tmux-plugins/yank/yank.tmux
    '';
  };

  modules.zsh.initExtraAfterCompInit = ''
    if test -z "$SKIP_TMUX" && command -v tmux >/dev/null && test -z "$TMUX"; then
      exec tmux new-session;
      exit;
    fi
  '';
}
