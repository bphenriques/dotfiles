{ pkgs, lib, ... }:
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  stylix.targets.zellij.enable = true;
  programs.zellij = {
    enable = true;
    themes.tmux = ./tmux-theme.kdl;

    settings = {
      default_mode = "locked"; # Locked mode by default: avoids keybinding collisions with Helix. Ctrl-g to unlock.
      session_serialization = false;
      default_layout = "compact";
      simplified_ui = true;

      # Suppress first-run wizard and release notes.
      show_startup_tips = false;
      show_release_notes = false;
    };
  };
}
