# Host-side microvm wiring on compute.
#
# Defines:
#   - The bridge `br-hermes` (10.20.1.0/24) connecting the host to its
#     guest VMs.
#   - NAT for VM egress through compute's uplink (`bond0`).
#   - The `hermes-vm` guest declaration pointing at
#     `nixosConfigurations.hermes-vm` in this flake.
#
# Phase 1 of the blue/green migration: blue (services/hermes.nix on
# compute) keeps running on port 8642; green (this VM, eth0 = 10.20.1.10)
# has no API server yet, accessed via `microvm -c hermes-vm` console for
# smoke testing. Once verified, phase 2 layers in sops-in-VM + API server
# + Traefik route and decommissions the host-side hermes-agent.
{ self, inputs, ... }:
let
  bridge = "br-hermes";
  hostBridgeIp = "10.20.1.1";
in
{
  imports = [ inputs.microvm.nixosModules.host ];

  # VMs that should start at boot. Without this, the unit is generated
  # but never enabled — the VM has to be hand-started after every reboot.
  microvm.autostart = [ "hermes-vm" ];

  # Narrow override of the fleet-wide sshd hardening (which disables
  # AllowTcpForwarding globally) — re-enable for the human user only,
  # so `ssh -J compute hermes@10.20.1.10` works to reach the microvm.
  # Root and all other accounts stay locked down per the shared profile.
  services.openssh.extraConfig = ''
    Match User bphenriques
      AllowTcpForwarding yes
  '';

  # Bridge interface — VMs attach here via tap interfaces.
  networking = {
    bridges.${bridge}.interfaces = [ ];
    interfaces.${bridge}.ipv4.addresses = [{
      address = hostBridgeIp;
      prefixLength = 24;
    }];

    # NAT for VM egress. Phase 1 allows all outbound; phase 2 narrows
    # to laptop:11434 + npm + named cloud APIs only.
    nat = {
      enable = true;
      internalInterfaces = [ bridge ];
      externalInterface = "bond0";
    };

    firewall.trustedInterfaces = [ bridge ];
  };

  # The wireguard-access nftables chain forwards with policy=drop (see
  # services/wireguard). Declare br-hermes as a trusted forward interface
  # so VM-to-LAN traffic isn't caught by the deny-by-default.
  custom.homelab.wireguard.trustedForwardInterfaces = [ bridge ];

  # Guest VM declaration. `flake = self` makes the guest's config live
  # in this same flake as nixosConfigurations.hermes-vm.
  #
  # `updateFlake` deliberately unset: compute is built from a laptop
  # checkout via `nixos-rebuild switch`, so VM updates flow through
  # that rebuild path. Setting `updateFlake` would point at a checkout
  # compute doesn't have, breaking `microvm -l`'s freshness check.
  microvm.vms.hermes-vm = {
    flake = self;
    restartIfChanged = true;
  };

  # Ensure the source directory for the host-secrets virtiofs share
  # exists at boot. virtiofsd starts independently of (and earlier than)
  # the homelab-secrets generator that *populates* this directory; if the
  # dir is missing, virtiofsd fails to create its socket and the VM dies
  # with "Failed to connect to hermes-vm-virtiofs-host-secrets.sock".
  # tmpfiles runs in stage 1, before any service.
  systemd.tmpfiles.rules = [
    "d /var/lib/microvm-secrets/hermes-vm 0755 root root - -"
  ];
}
