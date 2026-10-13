{ config, ... }:
{
  environment.variables.HOMEBREW_NO_ANALYTICS = "1";        # Avoid analytics
  homebrew = {
    enable = true;
    enableFishIntegration = true;
    enableZshIntegration = true;
    onActivation = {
      cleanup = "none"; # check and then zap
      upgrade = true;
    };
    taps = [ ];
    brews = [ ];
    casks = [
      "rectangle"         # Window Manager
      "intellij-idea-ce"
      "ghostty"
      "claude"
    ];
  };
}