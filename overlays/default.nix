{ inputs, ... }: {
  channels = _: prev: {
    stable = import inputs.nixpkgs { system = prev.system; }; # Unstable by default but with stable as option
  };
  customPackages =  _: prev: {
    dotfiles = inputs.self.packages.${prev.system}.dotfiles;
    frg = inputs.self.packages.${prev.system}.frg;
    ffd = inputs.self.packages.${prev.system}.ffd;
    preview = inputs.self.packages.${prev.system}.preview;

    fishPlugins = prev.fishPlugins.overrideScope (_: _: {
      dotfiles  = inputs.self.packages.${prev.system}.dotfilesFishPlugin;
      ffd       = inputs.self.packages.${prev.system}.ffdFishPlugin;
      frg       = inputs.self.packages.${prev.system}.frgFishPlugin;
    });
  };
  communityPackages = _: prev: {
    ghostty = inputs.ghostty.packages.${prev.system}.default;
  };
}
