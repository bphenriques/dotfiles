{ pkgs, lib, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  programs.zellij = {
    enable = true;

    settings = {
      default_mode = "locked"; # Locked mode by default: avoids keybinding collisions with Helix. Ctrl-g to unlock.
      session_serialization = false; # Dont use this feature.

      # Suppress first-run wizard and release notes.
      show_startup_tips = false;
      show_release_notes = false;

      ui.pane_frames.rounded_corners = true;
    };
  };
}
