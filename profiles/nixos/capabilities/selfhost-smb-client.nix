# Homelab SMB client: mounts the NAS shares and wires the sops credentials.
# Hosts only declare `selfhost.storage.mounts.smb.shares` (gids, optional uid).
{ config, lib, inputs, private, ... }:
let
  cfg = config.selfhost.storage.mounts.smb;
in
{
  imports = [ inputs.selfhost-nix.nixosModules.default ];

  selfhost.storage.mounts.smb = {
    enable = true;
    hostname = config.custom.fleet.lan.hosts.storage;
    credentialsPath = config.sops.templates."homelab-samba-credentials".path;
  };

  # A share named after someone in the household registry is theirs; everything else is a household share.
  custom.shares = lib.mapAttrs (name: mount: {
    root = mount.localMount;
    personal = private.users ? ${name};
  }) cfg.shares;

  sops = {
    secrets."homelab/samba/username" = { };
    secrets."homelab/samba/password" = { };
    templates."homelab-samba-credentials" = {
      owner = "root";
      group = "root";
      mode = "0400";
      content = ''
        username=${config.sops.placeholder."homelab/samba/username"}
        password=${config.sops.placeholder."homelab/samba/password"}
      '';
    };
  };
}
