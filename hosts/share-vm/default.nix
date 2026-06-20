# share-vm — public file-sharing guest, exposed to the internet only via Tailscale
# Funnel. A sealed cloud-hypervisor microVM: it owns its files and store on its own block
# devices, so compute shares nothing in. This module is the host shell (identity,
# admin access, secrets); the sharing service + metrics are in services/, exposure in
# funnel.nix, the VM hardware in microvm.nix.
{ config, pkgs, lib, inputs, private, ... }:
let
  fleet = import ../shared.nix;
  inherit (import ./lib.nix { inherit lib; }) dataRoot sshHostKey;
  adminUser = "bphenriques";
in
{
  imports = [
    inputs.microvm.nixosModules.microvm
    ./microvm.nix
    ./services
    ./funnel.nix
  ];

  system.stateVersion = "26.05";
  time.timeZone = "Europe/Lisbon";
  i18n.defaultLocale = "en_US.UTF-8";

  # Admin access only; reachable via `ssh -J compute bphenriques@<vm>`. Host key
  # persists on the volume (it is also the sops age identity).
  services.openssh = {
    enable = true;
    # Bridge IP only — not on loopback or tailscale0, so a localhost compromise can't
    # reach it and it's invisible to the tailnet (defence in depth on the gateway-only
    # firewall rule). The module orders sshd after network-online when this is set.
    listenAddresses = [{ addr = fleet.microvm.hosts.share-vm; port = 22; }];
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
      # Sealed box: no forwarding of any kind (the lean guest doesn't inherit the
      # fleet baseline). ProxyJump and sshfs need none of these.
      AllowTcpForwarding = false;
      AllowAgentForwarding = false;
      X11Forwarding = false;
    };
    hostKeys = [{ path = sshHostKey; type = "ed25519"; }];
    # Curation runs as the file owner: the laptop sshfs-mounts /srv/share as the
    # filebrowser user, restricted to SFTP only (no shell). This lets the upstream
    # FileBrowser module keep its own 0700/0077 hardening — we don't relax it to a group.
    extraConfig = ''
      Match User filebrowser
        ForceCommand internal-sftp
    '';
  };
  users.users.filebrowser.openssh.authorizedKeys.keys = fleet.authorizedSSHKeys;
  # Binding to a specific IP needs that IP up first, or sshd fails to start and locks
  # us out — wait for the bridge address (nixpkgs#105570).
  systemd.services.sshd = {
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
  };
  users.users.${adminUser} = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = fleet.authorizedSSHKeys;
  };
  security.sudo.wheelNeedsPassword = false;

  # sops uses the VM's SSH host key as its age identity. The key lives on the
  # volume, unmounted during initrd activation, so generate it in stage 2 (before
  # sshd claims it) and re-run activation once it exists — clean first boot.
  sops = {
    defaultSopsFile = private.sopsSecretsFile;
    age.sshKeyPaths = [ sshHostKey ];
    secrets."tailscale/authkey" = { };
  };
  systemd.services.ssh-host-key-init = {
    description = "Generate the SSH host key (sops age identity) on first boot";
    wantedBy = [ "multi-user.target" ];
    before = [ "sshd.service" "sops-late-activate.service" ];
    unitConfig = {
      ConditionPathExists = "!${sshHostKey}";
      RequiresMountsFor = [ dataRoot ];
    };
    serviceConfig.Type = "oneshot";
    path = [ pkgs.openssh ];
    script = ''
      mkdir -p ${dataRoot}/.ssh-host-keys
      ssh-keygen -t ed25519 -N "" -f ${sshHostKey}
    '';
  };
  systemd.services.sops-late-activate = {
    description = "Re-run activation once the volume (sops age key) is mounted";
    wantedBy = [ "multi-user.target" ];
    after = [ "ssh-host-key-init.service" ];
    before = [ "tailscaled-autoconnect.service" ];
    unitConfig.ConditionPathExists = sshHostKey;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "/run/current-system/activate";
    };
  };

  systemd.tmpfiles.rules = [ "d ${dataRoot}/.ssh-host-keys 0700 root root -" ];
}
