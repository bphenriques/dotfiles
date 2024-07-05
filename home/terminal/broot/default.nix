{ pkgs, ... }:
{
  # FIXME: Explore broot as a proper file navigation
  # TODO Explore: https://github.com/nix-community/home-manager/blob/master/modules/programs/broot.nix
  programs.broot = {
    enable = true;
    enableFishIntegration = true;
    settings.verbs = [
      {
        invocation = "create {subpath}";
        execution = "$EDITOR {directory}/{subpath}";
        leave_broot= false;
      }
      {
        invocation = "edit";
        shortcut = "e";
        execution= "$EDITOR +{line} {file}";
        leave_broot= false;
      }
    ];
  };
}
