_: {
  kavita-latest = import ./kavita;

  # Run `nix run .#check-updates` to check for newer upstream releases.
  pinned-github-releases = import ./jellyfin;
  pinned-container-images = import ./containers.nix;
}
