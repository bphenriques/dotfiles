{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    tmux
    tmuxPlugins.yank
  ];

  home.shellAliases = {
    reload      = "tmux respawn-pane -k";
  };

  xdg.configFile = {
    "tmux/tmux.conf".source        = ./tmux.conf;
    "tmux/tmux.theme.conf".source  = ./tmux.theme.conf;
    "tmux/tmux.plugins.conf".text  = ''
      run-shell ${pkgs.tmuxPlugins.yank}/share/tmux-plugins/yank/yank.tmux
    '';
  };

  modules.powerlevel10k.fastPrompt.beforeInit = ''
    if test -z "$SKIP_TMUX" && command -v tmux > /dev/null && test -z "$TMUX"; then
      exec tmux new-session;
      exit;
    fi
  '';
}
