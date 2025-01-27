{ lib, pkgs, config, ... }:
let
  groups = config.users.groups;
  users = config.users.users;
  
  mkCifsFs = hostname: remoteFolder: user: group: let
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
    device = "//${hostname}/${remoteFolder}";
    fsType = "cifs";
    options = (userOpts ++ credsOpts ++ networkSplitProtectionOpts);
  };
in
{
  environment.systemPackages = [ pkgs.cifs-utils ]; # Samba Server

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

  fileSystems = {
    "/mnt/nas-bphenriques"  = mkCifsFs "bruno-home-nas" "bphenriques" users.bphenriques groups.root;
    "/mnt/nas-media"        = mkCifsFs "bruno-home-nas" "media"       users.bphenriques groups.users;
    "/mnt/nas-shared"       = mkCifsFs "bruno-home-nas" "shared"      users.bphenriques groups.users;
  };

  systemd.tmpfiles.rules = [
    "z /mnt/nas-bphenriques   0700 ${users.bphenriques.name} ${groups.users.name}"
    "z /mnt/nas-media         0775 root                      ${groups.users.name}"
    "z /mnt/nas-shared        0775 root                      ${groups.users.name}"
  ];
}
