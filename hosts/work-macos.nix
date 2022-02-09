{ config, pkgs, lib, ... }:

{ 
  # MacOS specific settings
  imports = [
    ../macos/common.nix
    ../macos/work.nix
  ];

  # Setup Home-manager
  environment.shells = [ pkgs.zsh ];
  home-manager.users."brunohenriques" = {
    imports = [
      ../home/common.nix
      ../home/work.nix
    ];
  };

  # Ugly bit: This host uses Apple M1 Silicon Processor. This means that some apps have to be installed manually.
  system.activationScripts.postUserActivation.text = ''
    echo
    echo "Using Apple Silicon requires some apps to be manually installed:"
    echo "Manually install Docker: https://docs.docker.com/docker-for-mac/apple-silicon/"
    echo "Manually install IntelliJ: https://www.jetbrains.com/idea/download/#section=mac"
    echo
  '';
}
