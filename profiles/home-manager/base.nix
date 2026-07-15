_:
{
  imports = [ ./cli.nix ];

  # XDG compliance to tidy up $HOME.
  xdg.enable = true;
  home.preferXdgDirectories = true;

  # Discard home-manager's generated manual.
  manual = {
    html.enable = false;
    manpages.enable = false;
    json.enable = false;
  };
}
