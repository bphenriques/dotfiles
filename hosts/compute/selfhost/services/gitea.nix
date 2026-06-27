{ config, ... }:
{
  selfhost.apps.gitea = {
    enable = true;
    ssh.enable = true; #
  };
  networking.firewall.interfaces.bond0.allowedTCPPorts = [ config.selfhost.apps.gitea.ssh.port ]; # Avoid all interfaces
}
