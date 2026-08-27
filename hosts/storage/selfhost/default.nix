{
  config,
  inputs,
  lib,
  ...
}:
let
  # Passwords are this host's own secrets: the account lives here, not on the host the principal comes from.
  smbAccount = name: {
    enable = true;
    passwordFile = config.sops.secrets."samba/${name}-password".path;
  };

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

  # Others reach their files through compute's applications; only this one holds a direct SMB account.
  users.bphenriques = {
    email = "bphenriques@localhost"; # no OIDC and no mail here, so nothing reads it
    firstName = "Bruno";
    lastName = "Henriques";
    groups = [ config.selfhost.groups.users ];
    auth.oidc.enable = false;
  };
in
{
  imports = [
    inputs.selfhost-nix.nixosModules.default
    ./backup.nix
  ];

  # No services or ingress here: the backup pipeline, principal registries and SMB server all gate on this.
  selfhost.enable = true;

  # Ids pinned: these own files on a pool that outlives the root recording the allocation.
  selfhost.serviceAccounts = lib.mapAttrs (
    name: account: account // { storage.smb = smbAccount name; }
  ) serviceAccounts;

  selfhost.users = lib.mapAttrs (name: user: user // { storage.smb = smbAccount name; }) users;

  sops.secrets = lib.genAttrs (
    map (name: "samba/${name}-password") (lib.attrNames serviceAccounts ++ lib.attrNames users)
  ) (_: { restartUnits = [ "selfhost-smb-passwords.service" ]; });
}
