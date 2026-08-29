{
  config,
  inputs,
  lib,
  private,
  ...
}:
let
  # Passwords are this host's own secrets: the account lives here, not on the host the principal comes from.
  smbAccount = name: {
    enable = true;
    passwordFile = config.sops.secrets."samba/${name}-password".path;
  };

  # Everyone else reaches their files through compute's applications; only these hold a direct SMB account.
  smbPeople = [ "bphenriques" ];

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

  # No services or ingress here: the backup pipeline, principal registries and SMB server all gate on this.
  selfhost.enable = true;

  # One spelling of each group name: the framework canonical names come from the household vocabulary
  # rather than defaulting alongside it.
  selfhost.groups = { inherit (private.groups) admin users; };

  # Ids pinned: these own files on a pool that outlives the root recording the allocation.
  selfhost.serviceAccounts = lib.mapAttrs (
    name: account: account // { storage.smb = smbAccount name; }
  ) serviceAccounts;

  # The same registry compute reads, so group membership is decided in one place. Per-service config is
  # dropped: it belongs to the host running the service. A person with no SMB account is inert here.
  selfhost.users = lib.mapAttrs (
    name: person:
    removeAttrs person [ "services" ]
    // lib.optionalAttrs (lib.elem name smbPeople) { storage.smb = smbAccount name; }
  ) private.users;

  sops.secrets = lib.genAttrs (
    map (name: "samba/${name}-password") (lib.attrNames serviceAccounts ++ smbPeople)
  ) (_: { restartUnits = [ "selfhost-smb-passwords.service" ]; });
}
