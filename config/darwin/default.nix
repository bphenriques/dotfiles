{ pkgs, ... }:
{
  services.nix-daemon.enable = true;
  home-manager.useGlobalPkgs   = true;   # Use pkgs set within nixpkgs.
  home-manager.useUserPackages = true;   # Install packages defined in home-manager.

  environment.systemPackages = [ pkgs.fish ]; # Install the shell
  environment.shells = [ pkgs.fish ];         # Register the shell that was installed (2 step process)

  # System settings.
  # TODO: Check https://github.com/jwiegley/nix-config/blob/857ed3470073c34575e24ea37aba07760436ae30/config/darwin.nix#L300
  system = {
    screencapture.ensureLocation = true;
    defaults = {
      trackpad = {
        Clicking = true;                # Prefer tapping.
        TrackpadThreeFingerDrag = true; # Much more practical.
      };

      NSGlobalDomain = {
        # Style
        AppleInterfaceStyle = "Dark";               # Personal preference.
        _HIHideMenuBar      = false;                # Show top-bar.

        # Keybboard
        InitialKeyRepeat = 15;                      # Reduce the initial delay when pressing keys.
        KeyRepeat = 2;                              # Reduce delay when holding buttons.

        # Typing
        NSAutomaticSpellingCorrectionEnabled  = false;
        NSAutomaticCapitalizationEnabled      = false;
        NSAutomaticDashSubstitutionEnabled    = false;
        NSAutomaticPeriodSubstitutionEnabled  = false;
        NSAutomaticQuoteSubstitutionEnabled   = false;
      };

      dock = {
        mineffect = "genie";                # Animation.
        autohide = true;                    # Dont need the dock by default.
        tilesize = 30;                      # Small size.
        magnification = false;              # Dont magnify.
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

      SoftwareUpdate = {
        AutomaticallyInstallMacOSUpdates = false;   # Only when I want.
      };

      CustomSystemPreferences = {
        "com.apple.TextEdit" = {
          RichText = false;                         # Let me use plain-text.
        };

        "com.apple.screencapture" = {
          show-thumbnail = false;                   # Dont need the thumbnail.
          type = "png";                             # Higher quality.
        };

        # Avoid creating .DS_Store files on network or USB volumes
        "com.apple.desktopservices" = {
          DSDontWriteNetworkStores = true;
          DSDontWriteUSBStores = true;
        };

        "com.apple.finder" = {
          _FXSortFoldersFirst = true;               # Show folders first.
          _FXSortFoldersFirstOnDesktop = true;      # Show folders first on desktop.
          FXDefaultSearchScope = "SCcf";            # Search the current folder by default.
          ShowHardDrivesOnDesktop = false;          # Hide hard disks on desktop.
          ShowExternalHardDrivesOnDesktop = false;  # Hide external disks on desktop.
          ShowRemovableMediaOnDesktop = false;      # Hide removal media on desktop.
          ShowMountedServersOnDesktop = false;      # Hide mounted servers on desktop.
        };

        "com.apple.AdLib" = {
          allowApplePersonalizedAdvertising = false;  # Limit Apple personalized advertising
          allowAssistant = false;                     # I do not want Siri.
        };
        "com.apple.ImageCapture".disableHotPlug = true; # Stop Photos from automatically opening.
      };
    };

    keyboard = {
      enableKeyMapping = true;        # Ensure that I can change keys.
      remapCapsLockToControl = true;  # The key is more useful elsewhere.
    };
  };

  # Fonts (system-wide)
  fonts.packages = with pkgs; [
    (nerdfonts.override { fonts = [ "Hack" "JetBrainsMono" ]; })
  ];

  homebrew = {
    enable = true;
    onActivation = {
      cleanup = "zap";
      upgrade = true;
    };
    taps = [ ];
    brews = [ ];
    casks = [
      "rectangle"         # Window Manager
      "vlc"               # Media player
      "intellij-idea-ce"  # JVM IDE
      "ngrok"             # Useful
    ];
  };
}
