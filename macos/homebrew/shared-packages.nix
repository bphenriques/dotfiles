{
  homebrew.enable = true;
  homebrew.cleanup = "zap";
  
  imports = [
    ./emacs-package.nix
    ./jdk-packages.nix
  ];

  homebrew.taps = [
    "homebrew/cask"
    "batchcorp/public"          # For Plumber.
  ];

  homebrew.brews = [
    "dateutils"                 # Date utilities. Not supported currently by nix-pkgs.
    "plumber"                   # Useful utility for messaging queues.
  ];
  
  homebrew.casks = [
    "rectangle"                 # Window management.
    "alacritty"                 # Terminal.
    "vlc"                       # Media player.
  ];
}
