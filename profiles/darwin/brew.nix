_:
{
  environment.variables.HOMEBREW_NO_ANALYTICS = "1";
  homebrew = {
    enable = true;
    enableFishIntegration = true;
    enableZshIntegration = true;
    onActivation.cleanup = "uninstall";  # Not "zap", which also wipes app data
    casks = [
      "rectangle"         # Window Manager
      "intellij-idea"
      "ghostty"
      "claude"
    ];
  };
}
