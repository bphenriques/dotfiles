{ pkgs, ... }:

{
  # System settings.
  system = {
    defaults = {
      trackpad = {
        Clicking = true;                            # Because tapping is zappier.
        TrackpadThreeFingerDrag = true;             # Much more practical.
      };

      NSGlobalDomain = {
        AppleInterfaceStyle = "Dark";               # I prefer it.
        InitialKeyRepeat = 15;                      # Reduce the initial delay when pressing keys.
        KeyRepeat = 2;                              # Reduce delay when holding buttons.
        NSAutomaticCapitalizationEnabled = false;   # Let me handle capitalization.
      };

      SoftwareUpdate = {
        AutomaticallyInstallMacOSUpdates = false;   # Only when I want.
      };
    };

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;                # Sorry pal. Need that for more useful things.
    };
  };

  # Fonts
  fonts = {
    enableFontDir = true;
    fonts = [ (pkgs.nerdfonts.override { fonts = [ "Hack" ]; }) ]; # List here: https://github.com/ryanoasis/nerd-fonts
  };

  # Homebrew packages
  homebrew = {
    enable = true;
    cleanup = "zap";

    taps = [
      "homebrew/cask"             # Desktop Apps
      "batchcorp/public"          # For Plumber.
      "adoptopenjdk/openjdk"      # For JDK
    ];

    brews = [
      "dateutils"                 # Date utilities. Not supported currently by nix-pkgs
      "plumber"                   # Useful utility for messaging queues
    ];

    casks = [
      "rectangle"                 # Window management
      "alacritty"                 # Terminal. Too much hacks around keybindings and broken emacs experience
      "kitty"                     # Terminal. Experimenting this alternative
      "vlc"                       # Media player
      "firefox"                   # Browser
      "intellij-idea-ce"          # JVM IDE
      "keka"                      # Compression
      "adoptopenjdk8"             # JDK8
      "adoptopenjdk11"            # JDK11
    ];
  };
  imports = [ ./org-protocol ];
}
