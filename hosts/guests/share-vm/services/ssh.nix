# App-specific SSH: the base sshd (bridge listener, hardening, host-key bootstrap) is the
# microvm-guest.nix profile's; here only the SFTP curation account.
{ fleet, ... }:
{
  # SFTP-only (no shell): the laptop curates by sshfs-mounting as the file owner, so FileBrowser
  # keeps its own 0700 hardening. No group relaxation needed.
  services.openssh.extraConfig = ''
    Match User filebrowser
      ForceCommand internal-sftp
  '';
  users.users.filebrowser.openssh.authorizedKeys.keys = fleet.ssh.authorizedKeys;
}
