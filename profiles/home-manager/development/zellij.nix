{ pkgs, lib, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  programs.zellij = {
    enable = true;

    settings = {
      # Locked mode by default: avoids keybinding collisions with Helix (Ctrl-o, Ctrl-s, etc.).
      # Press Ctrl-g to unlock Zellij controls, then use shortcuts normally.
      default_mode = "locked";

      # Disable session serialization — Zellij is used for ephemeral workflows (e.g., fin edit), not persistent sessions.
      session_serialization = false;

      # Suppress first-run wizard and release notes.
      show_startup_tips = false;
      show_release_notes = false;

      ui.pane_frames.rounded_corners = true;
    };
  };
}
