{ config, lib, pkgs, self, ... }:
let
  # Edited by hand, out of Nix since the URLs are secret; it starts as the package's sample.
  calendars = "${config.xdg.configHome}/desktop-shell/calendars.yaml";
  eventsFile = "${config.xdg.stateHome}/calendar/events.json";
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  my.programs.desktop-shell.menu.entries = lib.mkOrder 900 [
    { key = "c"; desc = "Calendar"; icon = self.lib.glyphs."md-calendar"; width = 960; view = "calendar"; submenu = [
      { key = "s"; desc = "Sync"; icon = self.lib.glyphs."md-sync"; cmd = "systemctl --user start --no-block calendar-sync.service"; keepOpen = true; }
    ]; }
  ];
  my.programs.desktop-shell.settings.calendar.file = eventsFile;

  home.activation.calendars = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    [ -e "${calendars}" ] || run install -Dm600 ${self.packages.desktop-shell-calendar-sync.sample} "${calendars}"
  '';

  systemd.user = {
    # The shell can only see the file appear in a directory that exists when it starts.
    tmpfiles.rules = [ "d ${dirOf eventsFile} 0700 - - -" ];
    services.calendar-sync = {
      Unit.Description = "Syncs the calendars desktop-shell shows";
      Service = {
        Type = "oneshot";
        ExecStart = lib.escapeShellArgs [ (lib.getExe self.packages.desktop-shell-calendar-sync) calendars eventsFile ];
      };
    };
    timers.calendar-sync = {
      Install.WantedBy = [ "timers.target" ];
      Timer = {
        OnCalendar = "*:0/30";
        Persistent = true;
      };
    };
    # Editing the list syncs at once.
    paths.calendar-sync = {
      Install.WantedBy = [ "paths.target" ];
      Path.PathChanged = calendars;
    };
  };
}
