{ config, ... }:
{
  sops.secrets."users/bphenriques/hashedPassword".neededForUsers = true;

  # The POSIX identity of every SMB principal comes from ./selfhost; only this account also logs in.
  users.users.bphenriques = {
    hashedPasswordFile = config.sops.secrets."users/bphenriques/hashedPassword".path;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = config.fleet.ssh.authorizedKeys;
  };
}
