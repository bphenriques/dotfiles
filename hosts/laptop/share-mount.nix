# share-vm's files as an sshfs automount, like the NAS /mnt/homelab-* shares. Root mount by
# design: content is inert and curation is manual, so a dedicated non-root jump user isn't
# worth its cross-host key cost on a single-user desktop.
{ config, pkgs, ... }:
let
  user = config.users.users.bphenriques;
in
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
      # present FileBrowser's files as bphenriques so the desktop session owns them
      "allow_other"
      "default_permissions"
      "uid=${toString user.uid}"
      "gid=100"
      "reconnect"
      "ServerAliveInterval=15"
      "ServerAliveCountMax=3"
      "noexec"
      "nosuid"
      "nodev"
    ];
  };

  system.fsPackages = [ pkgs.sshfs ];
  programs.fuse.userAllowOther = true;

  # Key lives here, not as a mount option: root has no default key, and ProxyJump spawns a
  # separate ssh to compute that wouldn't inherit a mount-level IdentityFile.
  # accept-new, not yes: the automount runs as root with no TTY to accept a first-use prompt, so
  # first-connect TOFU is the seal (own wired LAN; a *changed* key is still refused). If share-vm's
  # host key regenerates (state wipe — it's the sops age identity), recover: `sudo ssh-keygen -R share-vm`.
  programs.ssh.extraConfig = ''
    Host compute share-vm
      IdentityFile ${user.home}/.ssh/id_ed25519
      StrictHostKeyChecking accept-new
    Host share-vm
      ProxyJump ${user.name}@compute
  '';
}
