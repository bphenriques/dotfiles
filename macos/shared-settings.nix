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
}
