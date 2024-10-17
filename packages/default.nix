{ nixpkgs, mylib }:
mylib.builders.forAllSystems (system:
  let pkgs = nixpkgs.legacyPackages.${system};
  in rec {
    dotfiles = pkgs.callPackage ./dotfiles { };
    preview = pkgs.callPackage ./preview { };
    fzf-rg = pkgs.callPackage ./fzf-rg { };
    fzf-fd = pkgs.callPackage ./fzf-fd { inherit preview; };
    project = pkgs.callPackage ./project { inherit preview; };
  }
)
