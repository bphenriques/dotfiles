{ inputs, ... }:
let
  mkFishPlugin = pkgs: pname: src: pkgs.buildFishPlugin {
    inherit pname src;
    version = "latest";
  };
in [
  (final: prev: {
    dotfiles = final.callPackage ./dotfiles {};
    frg = final.callPackage ./frg {};
    ffd = final.callPackage ./ffd {};
    preview = final.callPackage ./preview {};

    ghostty = inputs.ghostty.packages.${prev.system}.default;

    fishPlugins = prev.fishPlugins.overrideScope (finalx: prevx: {
      dotfiles = mkFishPlugin prevx "dotfiles" ./dotfiles/fish-plugin;
      ffd = mkFishPlugin prevx "ffd" ./ffd/fish-plugin;
      frg = mkFishPlugin prevx "frg" ./frg/fish-plugin;
    });
  })
  (final: prev: {
    ghostty = inputs.ghostty.packages.${prev.system}.default;
  })
  inputs.nur.overlay
]
