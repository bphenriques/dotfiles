{ config, pkgs, lib, ... }:
{
  users = {
    mutableUsers = false;
    users = {
      bphenriques = {
        isNormalUser = true;
        hashedPasswordFile = config.sops.secrets.bphenriques_password.path;
        description = "bphenriques";
        extraGroups = [ "wheel" "networkmanager" "docker" ];
      };
    };
  };
  users.users.bphenriques.shell = pkgs.fish;  # Fish is managed in Home-Manager. Keeping the default shell for root.
  home-manager.users.bphenriques = import ./bphenriques.nix;
}
