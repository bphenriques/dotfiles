{ config, ... }:
{
  sops.secrets."users/bphenriques/hashedPassword".neededForUsers = true;

  # The only SMB principal that also signs in, so the only one with a home and shell.
  users.users.bphenriques = {
    createHome = true;
    useDefaultShell = true;
    hashedPasswordFile = config.sops.secrets."users/bphenriques/hashedPassword".path;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = config.fleet.ssh.authorizedKeys;
  };
}
