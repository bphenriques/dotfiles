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
    "dateutils"                 # Date utilities. Not available in nix-pkgs.
    "plumber"                   # Useful utility for messaging queues.
    "gnupg2"                    # GPG. Consider using the one in nixpkgs.
    "pinentry-mac"              # GPG.
  ];
  
  homebrew.casks = [
    "rectangle"                 # Window management.
    "alacritty"                 # Terminal.
    "vlc"                       # Media player.
    "vscodium"                  # Open-Source counter-part of Microsoft Visual Studio Code
    "keybase"                   # E2E encrypted vauls and chats.
  ];
}
