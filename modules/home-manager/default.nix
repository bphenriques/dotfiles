{
  home-manager-programs-dotfiles = import ./programs/dotfiles.nix;
  home-manager-programs-proj = import ./programs/project.nix;
  home-manager-programs-fuzzy-fd = import ./programs/fuzzy-fd.nix;
  home-manager-programs-fuzzy-ripgrep = import ./programs/fuzzy-ripgrep.nix;
  home-manager-proton-run = import ./programs/proton-run.nix;
  home-manager-steam = import ./programs/steam.nix;
  home-manager-lutris = import ./programs/lutris.nix;
  home-manager-xdg-mime-apps = import ./xdg-mime-apps.nix;
}
