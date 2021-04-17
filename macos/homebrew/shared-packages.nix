{
  homebrew.enable = true;

  imports = [
    ./emacs-package.nix
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
    "docker"                    # Containers.
    "intellij-idea-ce"          # IDE for JVM projects.
    #android-studio             # IDE for Android projects.
    "keybase"                   # E2E encrypted vauls and chats.
    "vlc"                       # Media player.
    "vscodium"                  # Open-Source counter-part of Microsoft Visual Studio Code
  ];
}
