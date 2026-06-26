# share-vm's files mounted like the NAS shares (/mnt/homelab-*): an sshfs automount over
# the existing admin SSH (key-based, laptop-only). The laptop is a stationary desktop, so
# this is effectively always available — it mounts on first access and reconnects. Curate
# from here onto the NAS. The (passphraseless) laptop key is already authorized for both hops.
#
# Deliberately a root mount: the content is inert (noexec,nosuid,nodev) and curation is by
# hand, so the only residual (a compromised VM's SFTP server exploiting the sftp *client*)
# is a speculative parser 0-day in a brief manual window — not worth a dedicated non-root
# user's cross-host key/jump cost, especially on a single-user desktop where sudo exists.
{ config, pkgs, ... }:
{
  fileSystems."/mnt/homelab-shared-vm" = {
    device = "filebrowser@share-vm:/srv/share";
    fsType = "fuse.sshfs";
    options = [
      "x-systemd.automount"
      "_netdev"
      "noauto"
      "x-systemd.idle-timeout=600"
      "x-systemd.mount-timeout=20s"
      # appear as bphenriques so the desktop session has full access to FileBrowser's files
      "allow_other"
      "default_permissions"
      "uid=${toString config.users.users.bphenriques.uid}"
      "gid=100"
      "reconnect"
      "ServerAliveInterval=15"
      "ServerAliveCountMax=3"
      # carries others' uploads — data, never an execution path here
      "noexec"
      "nosuid"
      "nodev"
    ];
  };
  # Provide the fuse mount helper; allow a non-root session to enter the root-owned mount.
  system.fsPackages = [ pkgs.sshfs ];
  programs.fuse.userAllowOther = true;

  # The whole connection lives here, read by root's mount and interactive ssh alike: the key
  # for each hop, host-key pinning, and the jump. ProxyJump spawns a *separate* ssh to compute
  # that wouldn't inherit a mount-level IdentityFile, and root has no default key — so the key
  # belongs in ssh_config, where every hop's ssh finds it.
  programs.ssh.extraConfig = ''
    Host compute share-vm
      IdentityFile ${config.users.users.bphenriques.home}/.ssh/id_ed25519
      StrictHostKeyChecking yes
    Host share-vm
      ProxyJump bphenriques@compute
  '';
}
