# Homelab SMB client: mounts the NAS shares and wires the sops credentials.
# Hosts only declare `custom.homelab.smb.mounts` (gids, optional uid).
{ config, lib, ... }:
let
  cfg = config.custom.homelab.smb;
  hasMount = name: cfg.mounts ? ${name};
in
{
  custom.homelab.smb = {
    enable = true;
    hostname = config.custom.fleet.lan.hosts.bruno-home-nas;
    credentialsPath = config.sops.templates."homelab-samba-credentials".path;
  };

  custom.homelab.paths = lib.mkMerge [
    (lib.mkIf (hasMount "media") { media.root = cfg.mounts.media.localMount; })
    (lib.mkIf (hasMount "bphenriques") { users.bphenriques.root = cfg.mounts.bphenriques.localMount; })
  ];

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
