# The NAS doors this guest's principal is granted (hosts/storage/services/samba.nix), mounted with its own credential.
{ config, alfredoVm, inputs, ... }:
{
  imports = [ inputs.selfhost-nix.nixosModules.default ];

  selfhost.storage.mounts.smb = {
    enable = true;
    hostname = config.fleet.lan.hosts.storage;
    credentialsPath = config.sops.templates."smb-credentials".path;
    shares = {
      notes = {
        localMount = alfredoVm.vaultRoot;
        group = "vault";
        gid = 5000;
        systemd.dependentServices = [ "hermes-agent" ];
      };
      recipes = {
        localMount = alfredoVm.recipesRoot;
        group = "recipes";
        gid = 5001;
        systemd.dependentServices = [ "hermes-agent" "cookidoo-sync" ];
      };
    };
  };

  sops.secrets."smb/password" = { };
  sops.templates."smb-credentials".content = ''
    username=machine-agent-alfredo-vm
    password=${config.sops.placeholder."smb/password"}
  '';
}
