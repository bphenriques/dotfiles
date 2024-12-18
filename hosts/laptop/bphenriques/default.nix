{ config, pkgs, ... }:
{
  sops.secrets.user_bphenriques_password.neededForUsers = true;
  users.users.bphenriques = {
    description = "Bruno Henriques";
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "networkmanager" "docker" ];
    hashedPasswordFile = config.sops.secrets.user_bphenriques_password.path;
    shell = pkgs.fish;
  };
  nix.settings.trusted-users = [ "bphenriques" ];

  home-manager.users.bphenriques = import ./home.nix;
}
