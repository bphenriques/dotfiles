{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    tmux
    tmuxPlugins.yank
  ]; # Not using `enable: true` as I manage by my own config.

  xdg.configFile = {
    "tmux/tmux.conf".source        = ./tmux.conf;
    "tmux/tmux.theme.conf".source  = ./tmux.theme.conf;
    "tmux/tmux.plugins.conf".text  = ''
      run-shell ${pkgs.tmuxPlugins.yank}/share/tmux-plugins/yank/yank.tmux
    '';
  };
}
