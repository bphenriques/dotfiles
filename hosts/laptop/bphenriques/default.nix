{ lib, config, ... }:
{
  sops.secrets.user_bphenriques_password.neededForUsers = true;
  users.users.bphenriques = {
    isNormalUser = true;
    uid = 1000;
    hashedPasswordFile = config.sops.secrets.user_bphenriques_password.path;
    extraGroups = [ "wheel" ]
      ++ lib.optionals config.networking.networkmanager.enable  [ "networkmanager" ]
      ++ lib.optionals config.virtualisation.docker.enable      [ "docker" ];
  };

  home-manager.users.bphenriques = import ./home.nix;
}
