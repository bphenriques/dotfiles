_: {
  # Run `nix run .#check-updates` to check for newer upstream releases.
  pinned-github-releases = import ./github-releases.nix;
  pinned-container-images = import ./containers.nix;
  open-webui-image-edit = import ./open-webui-image-edit.nix;
  open-webui-mcp1 = import ./open-webui-mcp1.nix;
  mcp-servers = import ./mcp-servers.nix;
  posthog-mcp2 = import ./posthog-mcp2.nix;
  romm = import ./romm.nix;
}
