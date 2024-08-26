{ pkgs, ... }:
{
  # TODO Explore: https://github.com/nix-community/home-manager/blob/master/modules/programs/yazi.nix
  programs.yazi = {
    enable = true;
    enableFishIntegration = true;
  };
}
