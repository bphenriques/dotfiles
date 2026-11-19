# The agent's guest as the host that runs it says: its SSH name, its API's port, where it keeps the API key, and the sync that brings the agenda's changes here.
{ lib, pkgs, writeNushellScript, sshHost ? "agent-alfredo-vm", apiPort ? 8642, apiKeyFile ? "/run/secrets/hermes/api-key", syncCommand ? "" }:
let
  script = writeNushellScript "alfredo" ./script.nu;
in
pkgs.writeShellApplication {
  name = "alfredo";
  runtimeInputs = [ pkgs.nushell pkgs.openssh pkgs.imagemagick ];
  runtimeEnv = {
    SSH_HOST = sshHost;
    API_PORT = toString apiPort;
    API_KEY_FILE = apiKeyFile;
    SYNC = syncCommand;
  };
  text = ''
    if [ "''${1:-}" != ask ]; then exec nu ${script} "$@"; fi

    # quick-key stops a turn by ending this command, but the run lives on the guest: stop it there, once `ask` has named it.
    RUN_FILE=$(mktemp)
    export RUN_FILE
    trap 'rm -f "$RUN_FILE"' EXIT
    stop() {
      until [ -s "$RUN_FILE" ] || ! kill -0 "$turn" 2>/dev/null; do sleep 0.1; done
      if [ -s "$RUN_FILE" ]; then nu ${script} stop "$(cat "$RUN_FILE")"; fi
    }
    nu ${script} "$@" <&0 &
    turn=$!
    trap stop TERM
    while kill -0 "$turn" 2>/dev/null; do wait "$turn" || true; done
  '';
  meta.platforms = lib.platforms.all;
}
