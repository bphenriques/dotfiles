# Adds a task through Alfredo, who reads a day or a time out of its text; without him, as written. The laptop says how to reach both.
{ lib, pkgs, writeNushellScript, alfredo ? "alfredo", sync ? "", fallback ? "", tasksFile ? "" }:
let
  script = writeNushellScript "alfredo-task" ./script.nu;
in
pkgs.writeShellApplication {
  name = "alfredo-task";
  runtimeInputs = [ pkgs.nushell pkgs.coreutils ];
  runtimeEnv = {
    ALFREDO = alfredo;
    SYNC = sync;
    FALLBACK = fallback;
    TASKS_FILE = tasksFile;
  };
  text = ''
    exec nu ${script} "$@"
  '';
  meta.platforms = lib.platforms.linux;
}
