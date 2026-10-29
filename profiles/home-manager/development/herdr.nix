{ config, lib, pkgs, ... }:
let
  claudeDir = config.programs.claude-code.configDir;
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  programs.herdr = {
    enable = true;
    settings.onboarding = false; # nix owns the install, so the first-run wizard has nothing left to set up
  };

  # Lets the agents drive herdr itself: list panes, read another agent, wait on a state. The sidebar needs
  # none of this; it is for asking one agent to check on another.
  home.file = {
    "${claudeDir}/skills/herdr/SKILL.md".source = "${pkgs.herdr}/share/skills/herdr/herdr/SKILL.md";
    ".omp/agent/skills/herdr/SKILL.md".source = "${pkgs.herdr}/share/skills/herdr/herdr/SKILL.md";
  };

  # What makes the sidebar authoritative: each agent reports its own lifecycle rather than herdr reading it
  # off the terminal. Both installers write into the agent's own config dir and refuse when it is missing,
  # and their contents come from the pinned herdr (stamped with an integration version), so this belongs on
  # every switch rather than being run once by hand. It will fail loudly if Claude's `settings.json` ever
  # becomes a store symlink, which is what happens the moment `programs.claude-code.settings` is set.
  home.activation.herdrIntegrations = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    run mkdir -p "${claudeDir}" "$HOME/.omp/agent/extensions"
    CLAUDE_CONFIG_DIR="${claudeDir}" run ${pkgs.herdr}/bin/herdr integration install claude
    run ${pkgs.herdr}/bin/herdr integration install omp
  '';
}
