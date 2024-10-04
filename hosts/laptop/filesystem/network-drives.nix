{ lib, pkgs, config, ... }:
let
  homeServerIp = "192.168.68.53";

  groups = config.users.groups;
  users = config.users.users;

  # Single-user mount for now. This should be reviewed to support multi-user as some folders are private.. but only when I have a new user.
  # See: https://4sysops.com/archives/linux-smb-mount-for-multiple-users/ and https://docs.redhat.com/en/documentation/red_hat_enterprise_linux/7/html/storage_administration_guide/mounting_an_smb_share#performing_a_multi-user_smb_mount
  mkHomeServerCifsFs = remoteFolder: user: group: let
    networkSplitProtectionOpts = [
      "x-systemd.automount"
      "noauto"
      "x-systemd.idle-timeout=15"
      "x-systemd.device-timeout=5s"
      "x-systemd.mount-timeout=5s"
    ];
    userOpts = [ "uid=${toString user.uid}" "gid=${toString group.gid}" ];
    credsOpts = [ "credentials=${config.sops.templates."smb-credentials".path}" ];
  in {
    device = "//${homeServerIp}/${remoteFolder}";
    fsType = "cifs"; # See https://nixos.wiki/wiki/Samba
    options = (userOpts ++ credsOpts ++ networkSplitProtectionOpts);
  };
in
{
  sops = {
    secrets.samba_server_username = { };
    secrets.samba_server_password = { };
    templates."smb-credentials" = {
      content = ''
        username=${config.sops.placeholder.samba_server_username}
        password=${config.sops.placeholder.samba_server_password}
      '';
    };
  };

  environment.systemPackages = [ pkgs.cifs-utils ]; # Samba Server
  fileSystems = {
    # NFS
    "/mnt/nas-bphenriques"  = mkHomeServerCifsFs "bphenriques" users.bphenriques groups.root;   # Private
    "/mnt/nas-media"        = mkHomeServerCifsFs "media"       users.bphenriques groups.users;  # Shared with others
    "/mnt/nas-shared"       = mkHomeServerCifsFs "shared"      users.bphenriques groups.users;  # Shared with others
  };

  # https://www.mankier.com/5/tmpfiles.d
  systemd.tmpfiles.rules = [
    "z /mnt/nas-bphenriques   0700 ${users.bphenriques.name} ${groups.users.name}"
    "z /mnt/nas-media         0775 root                      ${groups.users.name}"
    "z /mnt/nas-shared        0775 root                      ${groups.users.name}"
  ];
}
