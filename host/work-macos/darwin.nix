{
  homebrew = {
    taps = [
      "homebrew/cask"
      "snyk/tap"
      "int128/kubelogin"
    ];

    brews = [
      "kubectl"
      "snyk"                        # Security.
      "int128/kubelogin/kubelogin"  # K8s stuff
    ];

    casks = [
      "google-chrome" # Google Meet.
      "slack"         # The usual rabbit-hole of channels.
    ];
  };
}
