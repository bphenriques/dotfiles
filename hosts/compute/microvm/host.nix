{ config, lib, self, ... }:
let
  guests = import ./guests.nix;

  # agent-alfred-vm's NAS-backed virtiofs shares, by tag, and the SMB share each must sit under.
  alfredNasShares = {
    vault = "bphenriques";
    recipes = "media";
  };
  alfredShareSource = tag:
    (lib.findFirst (s: s.tag == tag) null self.nixosConfigurations.agent-alfred-vm.config.microvm.shares).source;
in
{
  imports = [ ./agent-alfred-vm-secret.nix ];

  my.microvm.host = {
    enable = true;
    uplink = "bond0";
    adminUser = config.users.users.bphenriques.name;
    inherit (guests) bridge guests;
  };

  # Without this virtiofsd can pin the empty mountpoint and hand the guest a share with nothing in it.
  selfhost.storage.mounts.smb.shares = lib.mapAttrs' (_: share:
    lib.nameValuePair share { systemd.dependentServices = [ "microvm-virtiofsd@agent-alfred-vm" ]; }
  ) alfredNasShares;

  # The guest names the host path it wants shared, and this host is what guards and mounts it. Nothing
  # links the two, so a renamed share would leave the guest pointing at a path that no longer exists
  # and only fail when virtiofsd starts. Fail at eval instead.
  assertions = lib.mapAttrsToList (tag: share:
    let
      source = alfredShareSource tag;
      shareRoot = config.fleet.shares.${share}.root;
    in
    {
      assertion = lib.hasPrefix "${shareRoot}/" source;
      message = ''
        agent-alfred-vm's "${tag}" share is ${source}, which is not under the ${share} share
        (${shareRoot}). Either point it back inside that share, or move the automount guard above
        to whichever share now holds it.
      '';
    }
  ) alfredNasShares;
}
