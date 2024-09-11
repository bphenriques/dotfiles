{ inputs, ... }:
[
  (final: prev: {
    dotfiles = inputs.self.packages.${prev.system}.dotfiles;
    frg = inputs.self.packages.${prev.system}.frg;
    ffd = inputs.self.packages.${prev.system}.ffd;
    preview = inputs.self.packages.${prev.system}.preview;

    fishPlugins = prev.fishPlugins.overrideScope (finalx: prevx: {
      dotfiles  = inputs.self.packages.${prev.system}.dotfilesFishPlugin;
      ffd       = inputs.self.packages.${prev.system}.ffdFishPlugin;
      frg       = inputs.self.packages.${prev.system}.frgFishPlugin;
    });
  })
  (final: prev: {
    ghostty = inputs.ghostty.packages.${prev.system}.default;
  })
  inputs.nur.overlay
]
