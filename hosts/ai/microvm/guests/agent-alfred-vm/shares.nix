# The NAS doors this guest's principal is granted (hosts/storage/services/samba.nix), mounted with its own credential.
{ config, fleet, alfredVm, inputs, ... }:
{
  imports = [ inputs.selfhost-nix.nixosModules.default ];

  selfhost.storage.mounts.smb = {
    enable = true;
    hostname = fleet.lan.hosts.storage;
    credentialsPath = config.sops.templates."smb-credentials".path;
    shares = {
      notes = {
        localMount = alfredVm.vaultRoot;
        group = "vault";
        gid = 5000;
        systemd.dependentServices = [ "hermes-agent" ];
      };
      recipes = {
        localMount = alfredVm.recipesRoot;
        group = "recipes";
        gid = 5001;
        systemd.dependentServices = [ "hermes-agent" "cookidoo-sync" ];
      };
    };
  };

  sops.secrets."smb/password" = { };
  sops.templates."smb-credentials".content = ''
    username=machine-agent-alfred-vm
    password=${config.sops.placeholder."smb/password"}
  '';
}
