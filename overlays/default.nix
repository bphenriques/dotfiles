{ inputs, ... }:
[
  # FIXME: flake.overlays.default = _: prev: self.packages.${prev.stdenv.hostPlatform.system} or { };
  (_: prev: {
    dotfiles = inputs.self.packages.${prev.system}.dotfiles;
    frg = inputs.self.packages.${prev.system}.frg;
    ffd = inputs.self.packages.${prev.system}.ffd;
    preview = inputs.self.packages.${prev.system}.preview;

    fishPlugins = prev.fishPlugins.overrideScope (_: _: {
      dotfiles  = inputs.self.packages.${prev.system}.dotfilesFishPlugin;
      ffd       = inputs.self.packages.${prev.system}.ffdFishPlugin;
      frg       = inputs.self.packages.${prev.system}.frgFishPlugin;
    });
  })
  (_: prev: {
    ghostty = inputs.ghostty.packages.${prev.system}.default;
  })
  inputs.nur.overlay
]
