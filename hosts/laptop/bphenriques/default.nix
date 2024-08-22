{ config, pkgs, ... }:
{
  users.users.bphenriques.extraGroups = [ "wheel" "networkmanager" "docker" ];
  users.users.bphenriques.shell = pkgs.fish;
  home-manager.users.bphenriques = import ./home.nix;
}
