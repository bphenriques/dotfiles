# Host-agnostic sealed-guest contract: v4-only net, tap/vsock, node metrics, ingress firewall, SSH
# over the bridge, and the sops-age-identity-is-the-host-key bootstrap. `guestPlacement` injected by the host.
{ config, lib, pkgs, inputs, guestPlacement, private, fleet, ... }:
let
  cfg = config.homelab.microvm.guest;
  tapId = "vm-${config.networking.hostName}";     # matches the host's vm-* enslave glob
  hostKeyDir = "${cfg.stateRoot}/.ssh-host-keys";
  sshHostKey = "${hostKeyDir}/ssh_host_ed25519_key";
in
{
  imports = [
    inputs.microvm.nixosModules.microvm
    inputs.sops-nix.nixosModules.sops
  ];

  options.homelab.microvm.guest = {
    enable = lib.mkEnableOption "sealed microVM guest wiring (net, metrics, ssh, host-key bootstrap)";
    stateRoot = lib.mkOption {
      type = lib.types.str;
      description = "Persistent state dir holding the SSH host key (doubles as the sops age identity).";
    };
    dns = lib.mkOption { type = lib.types.str; default = fleet.dns; };
    ingressPorts = lib.mkOption {
      type = lib.types.listOf lib.types.port;
      default = [ ];
      description = "Extra TCP ports reachable from the host bridge, beyond 22 + node-exporter.";
    };
  };

  config = lib.mkIf cfg.enable {
    networking = { useDHCP = false; useNetworkd = true; };
    systemd.network = {
      enable = true;
      networks."10-lan" = {
        matchConfig.MACAddress = guestPlacement.mac;   # virtio gives unpredictable enp0sN names; match by MAC
        networkConfig = {
          Address = "${guestPlacement.ip}/${toString guestPlacement.prefixLength}";
          Gateway = guestPlacement.gateway;
          DNS = cfg.dns;
          LinkLocalAddressing = "ipv4";   # v4-only guest: no fe80:: and no RA-assigned v6, so the
          IPv6AcceptRA = false;           # host's v4-only LAN-drop seal can't be sidestepped over v6
        };
      };
    };

    # Sealed appliance resolves via static DNS; drop the link-local LLMNR responder.
    services.resolved.settings.Resolve.LLMNR = false;

    microvm.interfaces = [{ type = "tap"; id = tapId; inherit (guestPlacement) mac; }];
    microvm.vsock.cid = guestPlacement.vsockCid;   # for readiness systemd integration

    services.prometheus.exporters.node = {
      enable = true;
      listenAddress = guestPlacement.ip;   # bridge IP (host-only behind the firewall)
      port = 9100;
      openFirewall = false;
    };

    networking.nftables.enable = true;
    networking.firewall = {
      enable = true;
      extraInputRules = "ip saddr ${guestPlacement.gateway} tcp dport { 22, 9100${lib.concatMapStrings (p: ", ${toString p}") cfg.ingressPorts} } accept";
    };

    services.openssh = {
      enable = true;
      listenAddresses = [{ addr = guestPlacement.ip; port = 22; }];   # bridge only, not localhost/tailnet
      settings = {
        PasswordAuthentication = false;
        PermitRootLogin = "no";
        AllowTcpForwarding = false;
        AllowAgentForwarding = false;
        X11Forwarding = false;
      };
      hostKeys = [{ path = sshHostKey; type = "ed25519"; }];
    };
    # SSHD must wait for the IP or it fails: https://github.com/NixOS/nixpkgs/issues/105570
    systemd.services.sshd = {
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
    };

    # sops's age identity IS the VM's SSH host key, so the state volume joins the initrd and an
    # activation step generates the key (if absent) before sops runs.
    fileSystems.${cfg.stateRoot}.neededForBoot = true;
    sops = {
      defaultSopsFile = private.sopsSecretsFile;
      age.sshKeyPaths = [ sshHostKey ];
    };
    system.activationScripts = {
      sshHostKeyInit.text = ''
        if [ ! -e ${sshHostKey} ]; then
          install -d -m 700 ${hostKeyDir}
          ${pkgs.openssh}/bin/ssh-keygen -t ed25519 -N "" -f ${sshHostKey}
        fi
      '';
      setupSecrets.deps = [ "sshHostKeyInit" ];
    };
  };
}
