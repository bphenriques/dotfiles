# Host-side microvm wiring on compute.
#
# Defines:
#   - The `compute-microvm` bridge connecting compute to its guest VMs.
#   - NAT for VM egress through compute's uplink (`bond0`).
#   - The `hermes-vm` guest declaration pointing at
#     `nixosConfigurations.hermes-vm` in this flake.
#
# Bridge name, network, and VM IPs are pinned in `hosts/shared.nix`
# (`custom.fleet.microvm.*`) so the VM and host agree without duplication.
{ self, config, inputs, ... }:
let
  inherit (config.custom.fleet.microvm) bridge;
in
{
  imports = [ inputs.microvm.nixosModules.host ];

  # VMs that should start at boot. Without this, the unit is generated
  # but never enabled — the VM has to be hand-started after every reboot.
  microvm.autostart = [ "hermes-vm" ];

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

  # gitea-configure on compute uses this to set hermes-agent's gitea login password.
  # The VM doesn't see it — vault-sync authenticates via a PAT (see hermes-agent/gitea-token in the VM).
  sops.secrets."hermes-agent/gitea-password" = {
    owner = "gitea";
    mode = "0440";
  };

  # Gitea account for hermes-agent. Provisioned by gitea-configure from the
  # password secret above. Repo-level read permission is granted by hand in the Gitea UI.
  custom.homelab.users.hermes-agent = {
    email = "hermes-agent@localhost";
    firstName = "Hermes";
    lastName = "Agent";
    groups = [ ];
    services = {
      oidc.enable = false;
      gitea = {
        enable = true;
        passwordFile = config.sops.secrets."hermes-agent/gitea-password".path;
      };
    };
  };
}
