{
  homebrew.enable = true;
  #homebrew.cleanup = "zap";

  # TODO: https://github.com/macalinao/dotfiles/blob/4227de3084c7abcaf1139a1b49c9c1a2472a2a6f/nix/darwin/default.nix#L19
  # if isM1 then "/opt/homebrew/bin" else "/usr/local/bin";
  homebrew.brewPrefix = "/opt/homebrew/bin"; # "/usr/local/bin" for intel cpu
  
  # Consider the following for personal stuff: https://github.com/macalinao/dotfiles/blob/4227de3084c7abcaf1139a1b49c9c1a2472a2a6f/nix/darwin/default.nix#L50

  imports = [
    ./emacs-package.nix
    ./jdk-packages.nix
  ];

  homebrew.taps = [
    "homebrew/cask"
    "batchcorp/public"          # For Plumber.
  ];

  homebrew.brews = [
    "wget"
    "dateutils"                 # Date utilities. Not supported currently by nix-pkgs.
    "plumber"                   # Useful utility for messaging queues.
  ];
  
  homebrew.casks = [
    "rectangle"                 # Window management.
    "alacritty"                 # Terminal.
    "vlc"                       # Media player.
    "vscodium"                  # Open-Source counter-part of Microsoft Visual Studio Code
  ];

  system.activationScripts.postUserActivation.text = ''
    echo "----"
    echo "What is my CPU?"
    uname -m
    echo "----"
  '';
}
