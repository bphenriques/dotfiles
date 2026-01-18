{ lib, config, ... }:
{
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

  custom.home-server.users.bphenriques = {
    email = "bphenriques@example.com";
    name = "Bruno Henriques";
    services = {
      pocket-id = {
        enable = true;
        groups = [ "admins" "users" ];
      };
      immich.enable = true;
      obsidian-livesync = {
        enable = true;
        passwordFile = config.sops.secrets.obsidian_livesync_bphenriques_password.path;
        databases = [ "obsidiandb-bphenriques" ];
      };
    };
  };
  sops.secrets.obsidian_livesync_bphenriques_password = { };

  home-manager.users.bphenriques = import ./home.nix;
}
