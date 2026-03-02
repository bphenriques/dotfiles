{ lib, config, ... }:
{
  sops.secrets."users/bphenriques/hashedPassword".neededForUsers = true;
  users.users.bphenriques = {
    isNormalUser = true;
    uid = 1000;
    hashedPasswordFile = config.sops.secrets."users/bphenriques/hashedPassword".path; # mkpasswd --method=SHA-512 --stdin
    extraGroups = [ "wheel" ]
      ++ lib.optionals config.networking.networkmanager.enable  [ "networkmanager" ]
      ++ lib.optionals config.virtualisation.docker.enable      [ "docker" ]
      ++ lib.optionals config.custom.fileSystems.homelab.enable [
        "homelab-media"
        "homelab-bphenriques"
      ];
  };

  programs.git = {
    enable = true;
    config.safe.directory = [
      "/home/bphenriques/.dotfiles"
      "/home/bphenriques/.dotfiles-private"
    ];
  };

  home-manager.users.bphenriques = import ./home.nix;
}
