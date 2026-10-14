_:
{
  environment.variables.HOMEBREW_NO_ANALYTICS = "1";        # Avoid analytics
  homebrew = {
    enable = true;
    enableFishIntegration = true;
    enableZshIntegration = true;
    onActivation.cleanup = "uninstall";  # Not "zap", which also wipes app data
    taps = [ ];
    brews = [ ];
    casks = [
      "rectangle"         # Window Manager
      "intellij-idea"
      "ghostty"
      "claude"
    ];
  };
}
