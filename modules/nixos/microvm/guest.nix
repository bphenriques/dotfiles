# Host-agnostic sealed-guest contract: v4-only net, tap/vsock, node metrics, ingress firewall, SSH
# over the bridge, and the sops-age-identity-is-the-host-key bootstrap. Placement is set by mkMicrovmGuest.
{ config, lib, pkgs, inputs, private, fleet, ... }:
let
  cfg = config.custom.microvm.guest;

  tapId = "vm-${config.networking.hostName}";     # matches the host's vm-* enslave glob
  hostKeyDir = "${cfg.stateRoot}/.ssh-host-keys";
  sshHostKey = "${hostKeyDir}/ssh_host_ed25519_key";

  # Binding the guest IP fails outright if networkd has not configured it yet:
  # https://github.com/NixOS/nixpkgs/issues/105570. The budget outlasts wait-online's timeout so a
  # slow boot retries, while still reaching `failed` (and the alert) if the bind is genuinely broken.
  ipBoundService = {
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    startLimitIntervalSec = 120;
    startLimitBurst = 20;
    serviceConfig.RestartSec = "2s";
  };
in
{
  imports = [
    inputs.microvm.nixosModules.microvm
    inputs.sops-nix.nixosModules.sops
  ];

  options.custom.microvm.guest = {
    ip = lib.mkOption { type = lib.types.str; };
    mac = lib.mkOption { type = lib.types.str; };
    vsockCid = lib.mkOption { type = lib.types.int; };
    gateway = lib.mkOption { type = lib.types.str; };
    prefixLength = lib.mkOption { type = lib.types.int; };
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

  config = {
    networking = { useDHCP = false; useNetworkd = true; };
    systemd.network = {
      enable = true;
      # microvm.nix's optimize profile masks wait-online, which leaves network-online.target a no-op
      # and lets services ordered on it start before the address exists.
      wait-online = { enable = true; timeout = 30; };
      networks."10-lan" = {
        matchConfig.MACAddress = cfg.mac;   # virtio gives unpredictable enp0sN names; match by MAC
        linkConfig.RequiredForOnline = "routable";   # the default `degraded` is already met by the IPv4LL address
        networkConfig = {
          Address = "${cfg.ip}/${toString cfg.prefixLength}";
          Gateway = cfg.gateway;
          DNS = cfg.dns;
          LinkLocalAddressing = "ipv4";   # v4-only guest: no fe80:: and no RA-assigned v6, so the
          IPv6AcceptRA = false;           # host's v4-only LAN-drop seal can't be sidestepped over v6
        };
      };
    };

    # Rootfs is tmpfs, so Storage=auto picks "persistent" off a tmpfiles-made dir and logs into RAM.
    services.journald.settings.Journal.Storage = "volatile";

    microvm.interfaces = [{ type = "tap"; id = tapId; inherit (cfg) mac; }];
    microvm.vsock.cid = cfg.vsockCid;   # for readiness systemd integration

    services.prometheus.exporters.node = {
      enable = true;
      listenAddress = cfg.ip;   # bridge IP (host-only behind the firewall)
      port = 9100;
      openFirewall = false;
      enabledCollectors = [ "systemd" ];   # without it a failed unit is invisible from outside the guest
    };

    networking.nftables.enable = true;
    networking.firewall = {
      enable = true;
      extraInputRules = "ip saddr ${cfg.gateway} tcp dport { 22, 9100${lib.concatMapStrings (p: ", ${toString p}") cfg.ingressPorts} } accept";
    };

    services.openssh = {
      enable = true;
      openFirewall = false;   # its unqualified `tcp dport 22 accept` would shadow the rule above
      listenAddresses = [{ addr = cfg.ip; port = 22; }];   # bridge only, not localhost/tailnet
      settings.PermitRootLogin = "no";
      hostKeys = [{ path = sshHostKey; type = "ed25519"; }];
    };
    systemd.services.sshd = ipBoundService;
    systemd.services.prometheus-node-exporter = ipBoundService;

    # sops's age identity IS the VM's SSH host key, so the state volume joins the initrd and an
    # activation step generates the key (if absent) before sops runs.
    fileSystems.${cfg.stateRoot}.neededForBoot = true;
    # Wire sops only when the guest declares secrets; a secret-less guest needs no dotfiles-private entry.
    sops = lib.mkIf (config.sops.secrets != { } || config.sops.templates != { }) {
      defaultSopsFile = private.sopsSecretsFile;
      age.sshKeyPaths = [ sshHostKey ];
    };
    system.activationScripts.sshHostKeyInit.text = ''
      if [ ! -e ${sshHostKey} ]; then
        install -d -m 700 ${hostKeyDir}
        ${pkgs.openssh}/bin/ssh-keygen -t ed25519 -N "" -f ${sshHostKey}
      fi
    '';
    # Order sops's setupSecrets after the host key, but only when it exists: guard the whole entry so a
    # secret-less guest doesn't instantiate a textless activation script.
    system.activationScripts.setupSecrets =
      lib.mkIf (config.sops.secrets != { } || config.sops.templates != { }) { deps = [ "sshHostKeyInit" ]; };
  };
}
