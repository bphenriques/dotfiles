{ lib, pkgs, osConfig, ... }:
let
  romsDir = "${osConfig.custom.shares.media.root}/gaming/emulation/roms";
in
# SRM treats parser config as mutable runtime state. Do not manage it declaratively.
# Manual setup (one-time): close Steam → open SRM → create one Glob parser per system → set ROM directory,
#   executable, args, steam category, and local image paths (covers→tall, screenshots→hero, wheels→logo,
#   marquees→icon) → disable online image providers → save → close SRM →
#   systemctl --user start sync-steam-shortcuts.service
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [ pkgs.steam-rom-manager ];

  systemd.user = {
    # Re-run after ROM changes: systemctl --user start sync-steam-shortcuts.service (Steam must be closed)
    services.sync-steam-shortcuts = {
      Unit.Description = "Sync ROM shortcuts to Steam via Steam ROM Manager";
      Service = {
        Type = "oneshot";
        ExecCondition = "${lib.getExe (pkgs.writeShellApplication {
          name = "steam-shortcuts-ready";
          runtimeInputs = [ pkgs.procps ];
          text = ''
            if pgrep -x steam > /dev/null; then
              echo "Steam is running; skipping shortcut sync" >&2
              exit 1
            fi
            if ! test -d ${lib.escapeShellArg romsDir}; then
              echo "ROM directory is unavailable; skipping shortcut sync" >&2
              exit 1
            fi
          '';
        })}";
        # Nuke then re-add: ensures removed ROMs are cleaned up from Steam.
        ExecStart = [
          "${lib.getExe pkgs.steam-rom-manager} nuke"
          "${lib.getExe pkgs.steam-rom-manager} add"
        ];
      };
    };

    timers.sync-steam-shortcuts = {
      Unit.Description = "Sync Steam shortcuts daily (skips if Steam is open)";
      Install.WantedBy = [ "timers.target" ];
      Timer = {
        OnCalendar = "daily";
        RandomizedDelaySec = "1h";
        Persistent = false;
      };
    };
  };
}
