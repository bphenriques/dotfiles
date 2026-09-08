{ config, ... }:
{
  sops.secrets."users/bphenriques/hashedPassword".neededForUsers = true;

  users.users.bphenriques = {
    isNormalUser = true;
    linger = true;   # rootless containers are SIGKILLed on logout otherwise
    uid = 1000;
    hashedPasswordFile = config.sops.secrets."users/bphenriques/hashedPassword".path;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = config.custom.fleet.ssh.authorizedKeys;
  };
}
