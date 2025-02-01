{ nixpkgs, mylib, self }:
let
  lib = nixpkgs.lib;
  pkgsAttrToApps = attrs: lib.mapAttrs (_: pkg: { type = "app"; program = lib.getExe pkg; } ) attrs;

  crossPlatform = mylib.builders.forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      selfPkgs = self.packages.${system};
    in pkgsAttrToApps {
      post-install = pkgs.callPackage ./post-install { inherit selfPkgs; };
    });

  linux = mylib.builders.forLinuxSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      selfPkgs = self.packages.${system};
    in pkgsAttrToApps {
      nixos-install = pkgs.callPackage ./nixos-install { inherit selfPkgs; };
    });

  darwin = mylib.builders.forDarwinSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      selfPkgs = self.packages.${system};
    in pkgsAttrToApps {
      darwin-install = pkgs.callPackage { inherit selfPkgs; };
    });

in mylib.builders.mergeSystems [ crossPlatform linux darwin ]