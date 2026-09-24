_: {
  # Run `nix run .#check-updates` to check for newer upstream releases.
  pinned-github-releases = import ./jellyfin.nix;
  pinned-container-images = import ./containers.nix;
  open-webui-image-edit = import ./open-webui-image-edit.nix;
}
