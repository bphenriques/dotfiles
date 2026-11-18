# khal, ikhal and todo on the config quick-key's calendar sync keeps in `stateDir`, so they show what the shell does.
{ lib, pkgs, stateDir ? "$XDG_STATE_HOME/quick-key", ... }:
let
  wrap = name: exe: args: runtimeEnv: pkgs.writeShellApplication {
    inherit name runtimeEnv;
    text = ''exec ${exe} ${args} "$@"'';
  };
in
pkgs.symlinkJoin {
  name = "calendar-cli";
  paths = [
    (wrap "khal" (lib.getExe' pkgs.khal "khal") ''-c "${stateDir}/khal.conf"'' { })
    (wrap "ikhal" (lib.getExe' pkgs.khal "ikhal") ''-c "${stateDir}/khal.conf"'' { })
    # todoman loads its config as Python: no __pycache__ beside it.
    (wrap "todo" (lib.getExe' pkgs.todoman "todo") ''--config "${stateDir}/todoman.py"'' { PYTHONDONTWRITEBYTECODE = "1"; })
  ];
  meta.platforms = lib.platforms.linux;
}
