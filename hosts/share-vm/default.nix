{ config, pkgs, inputs, private, shareVm, ... }:
let
  fleet = import ../shared.nix;
  inherit (shareVm) dataRoot;
  sshHostKey = "${dataRoot}/.ssh-host-keys/ssh_host_ed25519_key";
  adminUser = "bphenriques";
in
{
  imports = [
    ./settings.nix
    inputs.microvm.nixosModules.microvm
    ./microvm.nix
    ./firewall.nix
    ./services
  ];

  time.timeZone = "Europe/Lisbon";
  i18n.defaultLocale = "en_US.UTF-8";

  services.openssh = {
    enable = true;
    # Bridge IP only: invisible to the tailnet and localhost — defence in depth over the
    # gateway-only firewall rule (./firewall.nix).
    listenAddresses = [{ addr = fleet.computeMicrovm.hosts.share-vm; port = 22; }];
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
      # No forwarding: ProxyJump/sshfs need none, and the lean guest skips the fleet baseline.
      AllowTcpForwarding = false;
      AllowAgentForwarding = false;
      X11Forwarding = false;
    };
    hostKeys = [{ path = sshHostKey; type = "ed25519"; }];
    # SFTP-only (no shell): the laptop curates by sshfs-mounting as the file owner, so
    # FileBrowser keeps its own 0700 hardening — no group relaxation needed.
    extraConfig = ''
      Match User filebrowser
        ForceCommand internal-sftp
    '';
  };
  users.users.filebrowser.openssh.authorizedKeys.keys = fleet.ssh.authorizedKeys;
  # Binding to a specific IP needs that IP up first, or sshd fails to start and locks
  # us out — wait for the bridge address (nixpkgs#105570).
  systemd.services.sshd = {
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
  };
  users.users.${adminUser} = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = fleet.ssh.authorizedKeys;
  };
  security.sudo.wheelNeedsPassword = false;

  # sops's age identity is the VM's SSH host key, on the state volume — so the volume joins
  # the initrd (neededForBoot) and an activation step generates the key (if absent) before
  # sops's own runs. One clean pass, first boot included; the bootstrap is in ./README.md.
  fileSystems.${dataRoot}.neededForBoot = true;
  sops = {
    defaultSopsFile = private.sopsSecretsFile;
    age.sshKeyPaths = [ sshHostKey ];
    secrets."tailscale/authkey" = { };
  };
  system.activationScripts = {
    sshHostKeyInit.text = ''
      if [ ! -e ${sshHostKey} ]; then
        install -d -m 700 ${dataRoot}/.ssh-host-keys
        ${pkgs.openssh}/bin/ssh-keygen -t ed25519 -N "" -f ${sshHostKey}
      fi
    '';
    setupSecrets.deps = [ "sshHostKeyInit" ];
  };

  system.stateVersion = "26.05";
}
