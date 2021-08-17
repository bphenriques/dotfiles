{ config, pkgs, lib, ... }:

{ 
  # MacOS specific settings
  imports = [
    # Shared Settings
    ../macos/shared-settings.nix
    ../macos/shared-fonts.nix
    ../macos/homebrew/shared-packages.nix

    # Host specific settings
    ../macos/homebrew/work-packages.nix
  ];

  # Apple Silicon
  homebrew.brewPrefix = "/opt/homebrew/bin";

  # Setup Home-manager
  home-manager.users."brunohenriques" = {
    imports = [ 
      # Shared Home Settings
      ../home/shared-home.nix 

      # Host specific
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
