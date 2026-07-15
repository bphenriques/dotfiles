{ lib, config, ... }:
let
  smbCfg = config.selfhost.storage.smb;
in
{
  sops.secrets."users/bphenriques/hashedPassword".neededForUsers = true;
  users.users.bphenriques = {
    isNormalUser = true;
    uid = 1000;
    hashedPasswordFile = config.sops.secrets."users/bphenriques/hashedPassword".path;
    extraGroups = [ "wheel" ] ++ lib.optionals smbCfg.enable (lib.mapAttrsToList (_: mount: mount.group) smbCfg.mounts);

    openssh.authorizedKeys.keys = config.custom.fleet.ssh.authorizedKeys;
  };

  # Defensive: keep ~/.ssh owner-only (was a home-manager tmpfiles rule; compute runs no home-manager).
  systemd.tmpfiles.rules = [ "z /home/bphenriques/.ssh 0700 bphenriques users" ];
}
