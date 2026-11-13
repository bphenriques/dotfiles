# The agent's guest as the host that runs it says: its SSH name, its API's port and where it keeps the API key.
{ lib, pkgs, writeNushellScript, sshHost ? "agent-alfredo-vm", apiPort ? 8642, apiKeyFile ? "/run/secrets/hermes/api-key" }:
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
  };
  text = ''
    exec nu ${script} "$@"
  '';
  meta.platforms = lib.platforms.all;
}
