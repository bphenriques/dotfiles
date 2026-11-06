{ config, self, ... }:
{
  home.packages = [ self.packages.dotfiles self.packages.dotfiles-secrets ];
  home.sessionVariables.DOTFILES_LOCATION = config.programs.nh.flake;
  programs.nh = {
    enable = true;
    flake = "${config.home.homeDirectory}/.dotfiles";
  };
}
