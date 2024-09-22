{ nixpkgs, mylib }:
mylib.builders.forAllSystems (system:
  let pkgs = nixpkgs.legacyPackages.${system};
  in rec {
    dotfiles = pkgs.callPackage ./dotfiles { };
    preview = pkgs.callPackage ./preview { };
    frg = pkgs.callPackage ./frg { };
    ffd = pkgs.callPackage ./ffd { inherit preview; };

    # FIXME: bundle it inside the above packages
    dotfilesFishPlugin = pkgs.callPackage ./dotfiles/fish.nix { };
    ffdFishPlugin = pkgs.callPackage ./ffd/fish.nix { };
    frgFishPlugin = pkgs.callPackage ./frg/fish.nix { };
  }
)
