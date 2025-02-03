{ nixpkgs, mylib, self }:
let
  lib = nixpkgs.lib;
  pkgsToApps = attrs: lib.mapAttrs (_: pkg: { type = "app"; program = lib.getExe pkg; } ) attrs;

  crossPlatform = mylib.builders.forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      selfPkgs = self.packages.${system};
    in pkgsToApps {
      post-install = pkgs.callPackage ./post-install { inherit selfPkgs; };
    });

  linux = mylib.builders.forLinuxSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      selfPkgs = self.packages.${system};
    in pkgsToApps {
      nixos-install = pkgs.callPackage ./nixos-install { inherit selfPkgs; };
    });

  darwin = mylib.builders.forDarwinSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      selfPkgs = self.packages.${system};
    in pkgsToApps {
      darwin-install = pkgs.callPackage { inherit selfPkgs; };
    });

in mylib.builders.mergeSystems [ crossPlatform linux darwin ]