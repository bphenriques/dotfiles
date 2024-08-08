{ config, pkgs, ... }:
{
  sops = {
    age.keyFile = "/persist/config/bphenriques/home/bphenriques/.config/sops/age/keys.txt"; # age-keygen
    defaultSopsFile = ./secrets/sops.yaml;
    secrets = {
      bphenriques_password.neededForUsers = true;  # echo "password" | mkpasswd -s
    };
  };
}
