{ config, lib, inputs, private, ... }:
let
  cfg = config.selfhost.storage.mounts.smb;
in
{
  imports = [ inputs.selfhost-nix.nixosModules.default ];

  selfhost.storage.mounts.smb = {
    enable = true;
    hostname = config.fleet.lan.hosts.storage;
    credentialsPath = config.sops.templates."homelab-samba-credentials".path;
  };

  # A share named after someone in the household registry is theirs; everything else is a household share.
  # The laptop carries no registry, so it marks nothing personal; nothing there reads the flag.
  fleet.shares = lib.mapAttrs (name: mount: {
    root = mount.localMount;
    personal = (private.users or { }) ? ${name};
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
