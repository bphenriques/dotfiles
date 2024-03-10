{ config, ... }:
let
  sopsKeysPath = "/home/${config.user.name}/.config/sops/age/keys.txt";
in
{
  sops = {
    age.keyFile = sopsKeysPath;
    defaultSopsFile = ./secrets/desktop.yaml;
  };

  assertions = [
    {
      assertion = !builtins.pathExists sopsKeysPath;
      message = "${sopsKeysPath} does not exist! This is part of the dotfiles setup.";
    }
  ];
}
