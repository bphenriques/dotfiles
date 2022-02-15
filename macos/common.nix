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
        # Style
        AppleInterfaceStyle = "Dark";               # Personal preference.

        # Keybboard
        InitialKeyRepeat = 15;                      # Reduce the initial delay when pressing keys.
        KeyRepeat = 2;                              # Reduce delay when holding buttons.

        # Typing experience.
        NSAutomaticSpellingCorrectionEnabled = false;
        NSAutomaticCapitalizationEnabled = false;
        NSAutomaticDashSubstitutionEnabled = false;
        NSAutomaticPeriodSubstitutionEnabled = false;
        NSAutomaticQuoteSubstitutionEnabled = false;
      };

      dock = {
        autohide = true;                    # Dont need the dock by default.
        tilesize = 30;                      # Small size.
        mru-spaces = false;                 # Dont rearrange spaces by most recently used.
        expose-group-by-app = true;         # Group by apps.
        show-recents = false;               # Dont show recent apps.
      };

      finder = {
        AppleShowAllExtensions = true;          # Show extensions in Finder by default.
        FXEnableExtensionChangeWarning = false; # Disable warning
      };

      screencapture = {
        location = "~/Pictures/screenshots";    # Avoid bloating the Desktop with screenshots.
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
