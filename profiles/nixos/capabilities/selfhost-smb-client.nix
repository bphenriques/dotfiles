# Homelab SMB client: mounts the NAS shares and wires the sops credentials.
# Hosts only declare `selfhost.storage.mounts.smb.shares` (gids, optional uid).
{ config, lib, inputs, ... }:
let
  cfg = config.selfhost.storage.mounts.smb;
  mountedRoot = name: lib.mkIf (cfg.shares ? ${name}) cfg.shares.${name}.localMount;
in
{
  imports = [ inputs.selfhost-nix.nixosModules.default ];

  selfhost.storage.mounts.smb = {
    enable = true;
    hostname = config.custom.fleet.lan.hosts.bruno-home-nas;
    credentialsPath = config.sops.templates."homelab-samba-credentials".path;
  };

  custom.shares = {
    bphenriques = {
      personal = true;
      root = mountedRoot "bphenriques";
    };
    media.root = mountedRoot "media";
    shared.root = mountedRoot "shared";
  };

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
