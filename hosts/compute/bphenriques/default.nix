{ lib, config, ... }:
{
  imports = [ ./homelab.nix ];

  sops.secrets.user_bphenriques_password.neededForUsers = true;
  users.users.bphenriques = {
    isNormalUser = true;
    uid = 1000;
    hashedPasswordFile = config.sops.secrets.user_bphenriques_password.path;
    extraGroups = [ "wheel" ]
      ++ lib.optionals config.networking.networkmanager.enable  [ "networkmanager" ]
      ++ lib.optionals config.virtualisation.docker.enable      [ "docker" ]
      ++ lib.optionals config.custom.fileSystems.homelab.enable [
        "homelab-media"
        "homelab-bphenriques"
      ];

    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBETAZZTh/Czemis4B6JKqySKLqWn5IUPqIvaJbEIe/3"
    ];
  };

  home-manager.users.bphenriques = import ./home.nix;
}
