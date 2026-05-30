# Host-side microvm wiring on compute.
#
# Defines:
#   - The `compute-microvm` bridge connecting compute to its guest VMs.
#   - NAT for VM egress through compute's uplink (`bond0`).
#   - The `personal-agent` guest declaration pointing at
#     `nixosConfigurations.personal-agent` in this flake.
#
# Bridge name, network, and VM IPs are pinned in `hosts/shared.nix`
# (`custom.fleet.microvm.*`) so the VM and host agree without duplication.
{ self, config, inputs, ... }:
let
  inherit (config.custom.fleet.microvm) bridge;
in
{
  imports = [ inputs.microvm.nixosModules.host ];

  microvm.autostart = [ "personal-agent" ];

  # Narrow override of the fleet-wide sshd hardening (which disables
  # AllowTcpForwarding globally) — re-enable for the human user only,
  # so `ssh -J compute bphenriques@<vm-ip>` works to reach the microvm.
  # Root and all other accounts stay locked down per the shared profile.
  services.openssh.extraConfig = ''
    Match User bphenriques
      AllowTcpForwarding yes
  '';

  # Bridge interface — VMs attach here via tap interfaces.
  networking = {
    bridges.${bridge.name}.interfaces = [ ];
    interfaces.${bridge.name}.ipv4.addresses = [{
      inherit (bridge) prefixLength;
      address = bridge.gateway;
    }];

    # NAT for VM egress.
    nat = {
      enable = true;
      internalInterfaces = [ bridge.name ];
      externalInterface = "bond0";
    };

    firewall.trustedInterfaces = [ bridge.name ];
  };

  # The wireguard-access nftables chain forwards with policy=drop (see
  # services/wireguard). Declare the microvm bridge as trusted forward
  # so VM-to-LAN traffic isn't caught by the deny-by-default.
  custom.homelab.wireguard.trustedForwardInterfaces = [ bridge.name ];

  # Guest VM declaration. `flake = self` makes the guest's config live
  # in this same flake as nixosConfigurations.personal-agent.
  #
  # `updateFlake` deliberately unset: compute is built from a laptop
  # checkout via `nixos-rebuild switch`, so VM updates flow through
  # that rebuild path. Setting `updateFlake` would point at a checkout
  # compute doesn't have, breaking `microvm -l`'s freshness check.
  microvm.vms.personal-agent = {
    flake = self;
    restartIfChanged = true;
  };

  # Gitea service account for vault-sync. gitea-configure generates a
  # throw-away random password on user creation; basic auth is disabled.
  # Auth from the VM is via the read-only PAT in the VM's sops file
  # (`hermes-agent/gitea-token` — see hosts/personal-agent/default.nix).
  # Repo-level read permission is granted by hand in the Gitea UI.
  custom.homelab.serviceAccounts.personal-agent = {
    description = "vault-sync identity for personal-agent";
    services.gitea.enable = true;
  };
}
