{ nixpkgs, mylib }:
mylib.builders.forAllSystems (system:
  let pkgs = nixpkgs.legacyPackages.${system};
  in rec {
    dotfiles = pkgs.callPackage ./dotfiles { };
    preview = pkgs.callPackage ./preview { };
    frg = pkgs.callPackage ./frg { };
    fuzzy-fd = pkgs.callPackage ./fuzzy-fd { inherit preview; };
    project = pkgs.callPackage ./project { inherit preview; };
  }
)
