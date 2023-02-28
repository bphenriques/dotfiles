{
  homebrew = {
    taps = [
      "homebrew/cask"
      "snyk/tap"
      "int128/kubelogin"
    ];

    brews = [
      "kubectl"
      "awscli"
      "snyk"                        # Security.
      "int128/kubelogin/kubelogin"  # Kubernetes
    ];

    casks = [
      "google-chrome" # Google Meet.
      "slack"         # The usual rabbit-hole of channels.
    ];
  };
}
