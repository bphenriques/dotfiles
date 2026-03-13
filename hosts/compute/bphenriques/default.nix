{ lib, config, self, ... }:
{
  sops.secrets."users/bphenriques/password".neededForUsers = true;
  users.users.bphenriques = {
    isNormalUser = true;
    uid = 1000;
    password = "password"; #temporary while I troubleshoot
    #hashedPasswordFile = config.sops.secrets."users/bphenriques/password".path;
    extraGroups = [ "wheel" ] ++ lib.optionals config.custom.homelab.smb.enable [
      "homelab-media"
      "homelab-bphenriques"
    ];

    openssh.authorizedKeys.keys = self.shared.authorizedSSHKeys;
  };

  home-manager.users.bphenriques = import ./home.nix;
}
