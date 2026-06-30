{ config, pkgs, inputs, private, shareVm, ... }:
let
  inherit (shareVm) dataRoot;
  sshHostKey = "${dataRoot}/.ssh-host-keys/ssh_host_ed25519_key";
  fleet = import ../../shared.nix;
in
{
  services.openssh = {
    enable = true;
    listenAddresses = [{
      addr = fleet.computeMicrovm.hosts.share-vm.ip; # Accessible within the bridge but not localhost nor tailnet.
      port = 22;
    }];
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
      # No forwarding: ProxyJump/sshfs need none, and the lean guest skips the fleet baseline.
      AllowTcpForwarding = false;
      AllowAgentForwarding = false;
      X11Forwarding = false;
    };
    hostKeys = [{ path = sshHostKey; type = "ed25519"; }];
    # SFTP-only (no shell): the laptop curates by sshfs-mounting as the file owner, so FileBrowser keeps its own 0700 hardening. No group relaxation needed.
    extraConfig = ''
      Match User filebrowser
        ForceCommand internal-sftp
    '';
  };
  users.users.filebrowser.openssh.authorizedKeys.keys = fleet.ssh.authorizedKeys;

  # SSHD needs to wait until the IP is up or it fails: https://github.com/NixOS/nixpkgs/issues/105570
  systemd.services.sshd = {
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
  };
}
