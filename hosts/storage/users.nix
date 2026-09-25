{ config, ... }:
{
  sops.secrets."users/bphenriques/hashedPassword".neededForUsers = true;

  # The POSIX identity of every SMB principal comes from ./selfhost, which grants no home and no shell.
  # This is the only account that also signs in, so it is the only one that asks for both.
  users.users.bphenriques = {
    createHome = true;
    useDefaultShell = true;
    hashedPasswordFile = config.sops.secrets."users/bphenriques/hashedPassword".path;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = config.fleet.ssh.authorizedKeys;
  };
}
