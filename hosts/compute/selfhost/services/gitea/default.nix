{ config, ... }:
{
  # App owns the git server, OIDC, CLI user/account provisioning and the backup hook; per-user opt-ins
  # are selfhost.users.<name>.apps.gitea, service accounts go on selfhost.apps.gitea.serviceAccounts.
  selfhost.apps.gitea = {
    enable = true;
    ssh.enable = true; # git-over-SSH; openFirewall left off so we scope the port to the LAN below
  };

  # Built-in SSH server reachable on the LAN only (not wg0), rather than the all-interfaces openFirewall.
  networking.firewall.interfaces.bond0.allowedTCPPorts = [ config.selfhost.apps.gitea.ssh.port ];
}
