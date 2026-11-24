# Alfredo's own terminal interface on his guest (`alfredo tui` there, in the gateway's environment), on a session or a new one; asking goes through quick-key's Ask.
{ lib, pkgs, sshHost ? "agent-alfredo-vm" }:
pkgs.writeShellApplication {
  name = "alfredo";
  runtimeInputs = [ pkgs.openssh ];
  runtimeEnv.SSH_HOST = sshHost;
  text = builtins.readFile ./open.sh;
  meta.platforms = lib.platforms.all;
}
