{
  home-manager-impermanence = import ./misc/impermanence.nix;
  home-manager-dotfiles = import ./misc/dotfiles.nix;
  home-manager-proton-run = import ./gaming/proton-run.nix;
  home-manager-steam = import ./gaming/steam.nix;
  home-manager-sunshine = import ./gaming/sunshine.nix;
  home-manager-lutris = import ./gaming/lutris.nix;
}
