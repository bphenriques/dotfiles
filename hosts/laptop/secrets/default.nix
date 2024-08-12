{ config, pkgs, ... }:
{
  sops = {
    age.keyFile = "${config.home-manager.users.bphenriques.custom.impermanence.dataLocation}/home/bphenriques/.config/sops/age/keys.txt";
    defaultSopsFile = ./sops.yaml;
    secrets = {
      bphenriques_password.neededForUsers = true;  # echo "password" | mkpasswd -s
    };
  };
}
