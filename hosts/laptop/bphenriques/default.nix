{ config, pkgs, ... }:
{
  users.users.bphenriques.extraGroups = [ "wheel" "networkmanager" "docker" ];
  users.users.bphenriques.shell = pkgs.fish;
  home-manager.users.bphenriques = import ./home.nix;

  # https://www.mankier.com/5/tmpfiles.d
  systemd.user.tmpfiles.rules = [
    "L /home/bphenriques/workdir  - - - - /mnt/bphenriques"
  ];
}
