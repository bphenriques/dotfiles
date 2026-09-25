{ config, lib, pkgs, inputs, private, fleet, ... }:
let
  cfg = config.my.microvm.guest;

  hostKeyDir = "${cfg.stateRoot}/.ssh-host-keys";
  sshHostKey = "${hostKeyDir}/ssh_host_ed25519_key";
  hasSecrets = config.sops.secrets != { } || config.sops.templates != { };
in
{
  imports = [
    inputs.microvm.nixosModules.microvm
    inputs.sops-nix.nixosModules.sops
  ];

  options.my.microvm.guest = {
    ip = lib.mkOption { type = lib.types.str; };
    mac = lib.mkOption { type = lib.types.str; };
    vsockCid = lib.mkOption { type = lib.types.int; }; # for readiness systemd integration
    gateway = lib.mkOption { type = lib.types.str; };
    prefixLength = lib.mkOption { type = lib.types.int; };
    stateRoot = lib.mkOption {
      type = lib.types.str;
      description = "Persistent state dir holding the SSH host key (doubles as the sops age identity).";
    };
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
      wait-online = { enable = true; timeout = 30; };   # microvm.nix masks it, leaving network-online.target a no-op
      networks."10-lan" = {
        matchConfig.MACAddress = cfg.mac;   # virtio gives unpredictable enp0sN names; match by MAC
        linkConfig.RequiredForOnline = "routable";   # the default `degraded` is already met by the IPv4LL address
        networkConfig = {
          Address = "${cfg.ip}/${toString cfg.prefixLength}";
          Gateway = cfg.gateway;
          DNS = fleet.dns;
          LinkLocalAddressing = "ipv4";   # v4-only guest: no fe80:: and no RA-assigned v6, so the
          IPv6AcceptRA = false;           # host's v4-only LAN-drop seal can't be sidestepped over v6
        };
      };
    };

    services.journald.settings.Journal.Storage = "volatile";   # tmpfs rootfs: Storage=auto would pick "persistent"

    microvm.interfaces = [{
      type = "tap";
      id = "vm-${config.networking.hostName}"; # matches the host's vm-* enslave glob
      inherit (cfg) mac;
    }];
    microvm.vsock.cid = cfg.vsockCid;

    services.prometheus.exporters.node = {
      enable = true;
      openFirewall = false;
      enabledCollectors = [ "systemd" ];   # without it a failed unit is invisible from outside the guest
    };

    networking.nftables.enable = true;
    networking.firewall.extraInputRules = "ip saddr ${cfg.gateway} tcp dport { 22, 9100${lib.concatMapStrings (p: ", ${toString p}") cfg.ingressPorts} } accept";

    # sshd and node-exporter listen on every address, so the rule above is the only gate.
    assertions = [{
      assertion = with config.networking.firewall; trustedInterfaces == [ "lo" ] && allowedTCPPorts == [ ] && allowedUDPPorts == [ ] && interfaces == { };
      message = "microvm guest ${config.networking.hostName}: firewall widened beyond the gateway-only rule, which is what keeps sshd and node-exporter off other interfaces";
    }];

    services.openssh = {
      enable = true;
      openFirewall = false;   # its unqualified `tcp dport 22 accept` would shadow the rule above
      hostKeys = [{ path = sshHostKey; type = "ed25519"; }];
    };

    fileSystems.${cfg.stateRoot}.neededForBoot = true; # Ensure identity is present when sops needs it
    sops = lib.mkIf hasSecrets {
      defaultSopsFile = private.sopsSecretsFile;
      age.sshKeyPaths = [ sshHostKey ];
    };
    system.activationScripts.sshHostKeyInit.text = ''
      if [ ! -e ${sshHostKey} ]; then
        install -d -m 700 ${hostKeyDir}
        ${pkgs.openssh}/bin/ssh-keygen -t ed25519 -N "" -f ${sshHostKey}
      fi
    '';
    system.activationScripts.setupSecrets = lib.mkIf hasSecrets { deps = [ "sshHostKeyInit" ]; };
  };
}
