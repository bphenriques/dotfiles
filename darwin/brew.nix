{ pkgs, lib, config, ... }:
{
  environment = {
    variables.HOMEBREW_NO_ANALYTICS = "1";        # Avoid analytics
    systemPath = [ config.homebrew.brewPrefix ];  # Add brew to $PATH
  };

  environment.extraInit = ''
    eval "$(${config.homebrew.brewPrefix}/brew shellenv)"
  '';

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
    ];
  };
}
