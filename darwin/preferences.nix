{ pkgs, ... }:
{
  system = {
    screencapture.ensureLocation = true;
    defaults = {
      NSGlobalDomain = {
        # Style
        AppleInterfaceStyle = "Dark";               # Personal preference.
        _HIHideMenuBar      = false;                # Show top-bar.

        # Typing
        NSAutomaticSpellingCorrectionEnabled  = false;
        NSAutomaticCapitalizationEnabled      = false;
        NSAutomaticDashSubstitutionEnabled    = false;
        NSAutomaticPeriodSubstitutionEnabled  = false;
        NSAutomaticQuoteSubstitutionEnabled   = false;
      };

      dock = {
        autohide = true;                    # Dont need the dock by default.
        orientation = "bottom";
        mineffect = "genie";                # Animation.
        tilesize = 30;                      # Small size.
        magnification = false;              # Dont magnify.
        mru-spaces = false;                 # Dont rearrange spaces by most recently used.
        expose-group-apps = true;           # Group by apps.
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
  };
}
