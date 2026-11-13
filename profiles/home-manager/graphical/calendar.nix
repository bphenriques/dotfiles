{ config, lib, pkgs, self, ... }:
let
  # Edited by hand, out of Nix since the URLs are secret; it starts as the package's sample.
  calendars = "${config.xdg.configHome}/desktop-shell/calendars.yaml";
  eventsFile = "${config.xdg.stateHome}/calendar/events.json";
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  my.programs.desktop-shell.menu.entries = lib.mkOrder 800 [
    { key = "c"; desc = "Calendar"; status = "calendar"; width = 960; view = "calendar"; submenu = [
      { key = "s"; desc = "Sync"; icon = self.lib.glyphs."md-sync"; cmd = "${lib.getExe' pkgs.systemd "systemctl"} --user start --no-block calendar-sync.service"; keepOpen = true; }
    ]; }
  ];
  my.programs.desktop-shell.settings.calendar = {
    file = eventsFile;
    remind = "${lib.getExe' pkgs.libnotify "notify-send"} --app-name=Calendar --icon=x-office-calendar";
  };

  # khal and ikhal in the terminal, on the config calendar-sync writes, so they show what the shell does.
  home.packages = map (name: pkgs.writeShellApplication {
    inherit name;
    runtimeInputs = [ pkgs.khal ];
    text = ''exec ${name} -c "${dirOf eventsFile}/khal.conf" "$@"'';
  }) [ "khal" "ikhal" ];

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
        # At login the network may not be up yet: retry, and never hang the next timer tick on a stalled connection.
        Restart = "on-failure";
        RestartSec = "1min";
        TimeoutStartSec = "5min";
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
