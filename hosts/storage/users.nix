{ config, ... }:
{
  sops.secrets."users/bphenriques/hashedPassword".neededForUsers = true;

  # Doubles as the SMB principal declared in ./selfhost: smbd resolves a session to this POSIX account.
  users.users.bphenriques = {
    isNormalUser = true;
    uid = 1000;
    hashedPasswordFile = config.sops.secrets."users/bphenriques/hashedPassword".path;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = config.custom.fleet.ssh.authorizedKeys;
  };
}
