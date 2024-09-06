{
  home-manager-dotfiles = import ./misc/dotfiles.nix;
  home-manager-proton-run = import ./gaming/proton-run.nix;
  home-manager-steam = import ./gaming/steam.nix;
  home-manager-lutris = import ./gaming/lutris.nix;
}
