{ pkgs, ... }:
{
  system = {
    defaults = {
      trackpad = {
        Clicking = true;                # Prefer tapping.
        TrackpadThreeFingerDrag = true; # Much more practical.
      };

      NSGlobalDomain = {
        InitialKeyRepeat = 15;    # Reduce the initial delay when pressing keys.
        KeyRepeat = 2;            # Reduce delay when holding buttons.
      };
    };

    keyboard = {
      enableKeyMapping = true;        # Ensure that I can change keys.
      remapCapsLockToControl = true;  # The key is more useful elsewhere.
    };
  };
}
