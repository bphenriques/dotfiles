{ lib, config, self, ... }:
{
  sops.secrets."users/bphenriques/password".neededForUsers = true;
  users.users.bphenriques = {
    isNormalUser = true;
    uid = 1000;
    hashedPasswordFile = config.sops.secrets."users/bphenriques/password".path;
    extraGroups = [ "wheel" ]
      ++ lib.optionals config.networking.networkmanager.enable  [ "networkmanager" ]
      ++ lib.optionals config.virtualisation.docker.enable      [ "docker" ]
      ++ lib.optionals config.custom.homelab.cifs.enable [
        "homelab-media"
        "homelab-bphenriques"
      ];

    openssh.authorizedKeys.keys = self.shared.authorizedSSHKeys;
  };

  home-manager.users.bphenriques = import ./home.nix;
}
