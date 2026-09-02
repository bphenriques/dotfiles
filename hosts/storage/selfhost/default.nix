{
  config,
  inputs,
  lib,
  private,
  ...
}:
let
  # The registry decides who holds an account; the password is this host's own secret, since the account
  # lives here rather than on the host the principal comes from.
  smbPeople = lib.filterAttrs (_: p: p.storage.smb.enable or false) private.users;
  smbPassword = name: config.sops.secrets."samba/${name}-password".path;

  serviceAccounts = {
    machine-compute = {
      description = "Compute's SMB principal";
      systemUser = {
        enable = true;
        uid = 999;
        gid = 999;
      };
    };
    machine-inky = {
      description = "Inky's read-only SMB principal";
      systemUser = {
        enable = true;
        uid = 998;
        gid = 998;
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
    enable = true;
    groups = { inherit (private.groups) admin users; };

    # Ids pinned: these own files on a pool that outlives the root recording the allocation.
    serviceAccounts = lib.mapAttrs (
      name: account:
      account
      // {
        storage.smb = {
          enable = true;
          passwordFile = smbPassword name;
        };
      }
    ) serviceAccounts;

    # The same registry compute reads, so membership is decided once. Per-service config belongs to the
    # host running the service; a person with no SMB account is inert here.
    users = lib.mapAttrs (
      name: person:
      removeAttrs person [ "services" ]
      // lib.optionalAttrs (smbPeople ? ${name}) {
        storage.smb = person.storage.smb // { passwordFile = smbPassword name; };
      }
    ) private.users;
  };

  sops.secrets = lib.genAttrs (
    map (name: "samba/${name}-password") (lib.attrNames serviceAccounts ++ lib.attrNames smbPeople)
  ) (_: { restartUnits = [ "selfhost-smb-passwords.service" ]; });
}
