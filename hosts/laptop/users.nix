{ config, pkgs, lib, ... }:
{
  users.mutableUsers = false;

  # bphenriques
  users.users.bphenriques = {
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets.bphenriques_password.path;
    description = "bphenriques";
    extraGroups = [ "wheel" "networkmanager" "docker" ];
    shell = pkgs.fish;  # Fish is managed in Home-Manager. Keeping the default shell for root.
  };
  home-manager.users.bphenriques = import ./bphenriques.nix;
  custom.home-remote-disks = {
    enable = false;
    smbCredentialsOwnerUsername = "bphenriques";
    uid = 1000;
    guid = 100;
    locations = [
      { mountPoint = "/home/bphenriques/nas"; device = "//${homeNasIp}/bphenriques"; }
      { mountPoint = "/mnt/nas-media";        device = "//${homeNasIp}/media"; }
      { mountPoint = "/mnt/nas-shared";       device = "//${homeNasIp}/shared"; }
    ];
  };
}
