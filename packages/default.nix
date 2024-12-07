{ nixpkgs, mylib }:

let
  inherit (mylib.builders) forAllSystems forLinuxSystems;
  crossPlatform = forAllSystems (system:
    let pkgs = nixpkgs.legacyPackages.${system};
    in rec {
      dotfiles = pkgs.callPackage ./dotfiles { };
      preview = pkgs.callPackage ./preview { };
      fzf-rg = pkgs.callPackage ./fzf-rg { };
      fzf-fd = pkgs.callPackage ./fzf-fd { inherit preview; };
      project = pkgs.callPackage ./project { inherit preview; };
    }
  );

  linux = forLinuxSystems (system:
    let pkgs = nixpkgs.legacyPackages.${system};
    in {
      dunst-volume = pkgs.callPackage ./dunst-volume { };
      dunst-brightness = pkgs.callPackage ./dunst-brightness { };
    }
  );
in forAllSystems (system:
  nixpkgs.lib.attrsets.mergeAttrsList [
    crossPlatform.${system}
    (linux.${system} or { })
  ]
)
