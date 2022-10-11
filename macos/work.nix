{
  homebrew = {
    taps = [ "homebrew/cask" ];

    brews = [
      "bazelisk"      # Build tool for a specific project.
    ];

    casks = [
      "google-chrome" # Google Meet.
      "slack"         # The usual rabbit-hole of channels.
    ];
  };
}
