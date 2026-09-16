{ config, lib, self, ... }:
let
  guests = import ./guests.nix;

  vaultShare = "bphenriques";
  vaultShareRoot = config.custom.shares.${vaultShare}.root;
  agentVaultSource =
    (lib.findFirst (s: s.tag == "vault") null self.nixosConfigurations.agent-vm.config.microvm.shares).source;
in
{
  imports = [
    ../../../profiles/nixos/capabilities/microvm-host.nix
    ./agent-vm-secret.nix
  ];

  homelab.microvm.host = {
    enable = true;
    uplink = "bond0";
    inherit (guests) bridge guests;
  };

  # agent-vm's vault share is the NAS mount: without this virtiofsd can pin the empty mountpoint and
  # hand the guest a vault with nothing in it.
  selfhost.storage.mounts.smb.shares.${vaultShare}.systemd.dependentServices = [
    "microvm-virtiofsd@agent-vm"
  ];

  # The guest names the host path it wants shared, and this host is what guards and mounts it. Nothing
  # links the two, so a renamed share would leave the guest pointing at a path that no longer exists
  # and only fail when virtiofsd starts. Fail at eval instead.
  assertions = [
    {
      assertion = lib.hasPrefix "${vaultShareRoot}/" agentVaultSource;
      message = ''
        agent-vm's "vault" share is ${agentVaultSource}, which is not under the ${vaultShare} share
        (${vaultShareRoot}). Either point it back inside that share, or move the automount guard above
        to whichever share now holds the vault.
      '';
    }
  ];
}
