{ lib, pkgs, osConfig, ... }:
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
      Unit = {
        Description = "Sync ROM shortcuts to Steam via Steam ROM Manager";
        RequiresMountsFor = [ osConfig.custom.homelab.paths.media.gaming.emulation.roms ];
      };
      Service = {
        Type = "oneshot";
        ExecCondition = "${lib.getExe (pkgs.writeShellApplication {
          name = "steam-not-running";
          runtimeInputs = [ pkgs.procps ];
          text = "! pgrep -x steam > /dev/null";
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
