{
  config,
  inputs,
  lib,
  private,
  ...
}:
let
  # Generated here, where samba validates them. Clients keep their own copy in their own secret store.
  serviceAccounts = {
    machine-compute = {
      description = "Compute's SMB principal";
      unixAccount = {
        enable = true;
        uid = 999;
        gid = 999;
      };
    };
    machine-inky = {
      description = "Inky's read-only SMB principal";
      unixAccount = {
        enable = true;
        uid = 998;
        gid = 998;
      };
    };
    machine-laptop = {
      description = "Laptop's SMB principal";
      unixAccount = {
        enable = true;
        uid = 978;
        gid = 978;
      };
    };
  };
in
{
  imports = [
    inputs.selfhost-nix.nixosModules.default
    ./backup.nix
  ];

  selfhost = {
    groups = { inherit (private.groups) admin users; };

    # Ids pinned: these own files on a pool that outlives the root recording the allocation.
    serviceAccounts = lib.mapAttrs (
      _name: account:
      account
      // {
        storage.smb.enable = true;
      }
    ) serviceAccounts;

    # The same registry compute reads, so membership is decided once. Per-service config belongs to the
    # host running the service; a person with no SMB account is inert here.
    users = lib.mapAttrs (
      _: person:
      lib.recursiveUpdate (removeAttrs person [ "services" ]) {
        # Opted in here, not in the shared registry: compute reads the same records and needs no accounts.
        unixAccount.enable = person.storage.smb.enable or false;
      }
    ) private.users;
  };
}
