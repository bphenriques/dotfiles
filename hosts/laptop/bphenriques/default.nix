{ config, pkgs, ... }:
{
  sops.secrets.user_bphenriques_password.neededForUsers = true;
  users.users.bphenriques = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "networkmanager" "docker" ];
    hashedPasswordFile = config.sops.secrets.user_bphenriques_password.path;
    shell = pkgs.fish;
  };

  home-manager.users.bphenriques = import ./home.nix;
}