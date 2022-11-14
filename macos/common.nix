{ pkgs, lib, config, ... }:

{
  environment.shells = [ pkgs.zsh ];  # Register the shell

  programs.org-protocol.enable = true;

  # System settings.
  system = {
    screencapture.createLocation = true;
    dock.picture = ./Day_at_Camp.png;

    defaults = {
      trackpad = {
        Clicking = true;                # Because tapping is zappier.
        TrackpadThreeFingerDrag = true; # Much more practical.
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
        mineffect = "genie";                # Animation.
        autohide = true;                    # Dont need the dock by default.
        tilesize = 30;                      # Small size.
        mru-spaces = false;                 # Dont rearrange spaces by most recently used.
        expose-group-by-app = true;         # Group by apps.
        show-recents = false;               # Dont show recent apps.
      };

      finder = {
        AppleShowAllExtensions = true;              # Show extensions in Finder by default.
        FXEnableExtensionChangeWarning = false;     # Disable warning when changing file extension.
        ShowPathbar = true;                         # Show breadcrumbs.
        FXPreferredViewStyle = "clmv";              # Show preview window by default.
      };

      screencapture = {
        location = "~/Pictures/screenshots";  # Avoid bloating the Desktop with screenshots.
      };

      SoftwareUpdate = {
        AutomaticallyInstallMacOSUpdates = false;   # Only when I want.
      };

      CustomSystemPreferences = {
        "com.apple.TextEdit" = {
          RichText = false;                         # Let me use plain-text.
        };

        "com.apple.screencapture" = {
          show-thumbnail = false;                   # Dont need the thumbnail.
        };

        "com.apple.finder" = {
          _FXSortFoldersFirst = true;               # Show folders first.
          _FXSortFoldersFirstOnDesktop = true;      # Show folders first on desktop.
          ShowHardDrivesOnDesktop = false;          # Hide hard disks on desktop.
          ShowExternalHardDrivesOnDesktop = false;  # Hide external disks on desktop.
          ShowRemovableMediaOnDesktop = false;      # Hide removal media on desktop.
          ShowMountedServersOnDesktop = false;      # Hide mounted servers on desktop.
        };
      };
    };

    keyboard = {
      enableKeyMapping = true;        # Ensure that I can change keys.
      remapCapsLockToControl = true;  # The key is more useful elsewhere.
    };
  };

  # Fonts
  fonts = {
    fontDir.enable = true;
    fonts = [ (pkgs.nerdfonts.override { fonts = [ "Hack" ]; }) ]; # List here: https://github.com/ryanoasis/nerd-fonts
  };

  # Homebrew packages
  homebrew = {
    enable = true;

    onActivation = {
      cleanup = "zap";
      upgrade = true;
    };

    taps = [
      "homebrew/cask"             # Desktop Apps
    ];

    brews = [
      "dateutils"                 # Date utilities. Not supported currently by nix-pkgs
    ];

    casks = [
      "rectangle"                 # Window management
      "kitty"                     # Terminal. Experimenting this alternative
      "vlc"                       # Media player
      "intellij-idea-ce"          # JVM IDE
      "keka"                      # Compression
      "rancher"                   # Docker Desktop alternative. Refer to https://github.com/rancher-sandbox/rancher-desktop/issues/1155#issuecomment-1007273576
      "temurin"                   # JDK: Successor of AdoptOpenJDK
    ];
  };
}
