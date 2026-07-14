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

  home-manager.users.bphenriques = {
    imports = [ ../../../profiles/home-manager ];
    home.stateVersion = "25.11";
  };
}
