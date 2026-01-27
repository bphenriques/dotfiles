{ nixpkgs, generators, self }:
let
  inherit (generators) forAllSystems forLinuxSystems forDarwinSystems mergeAllSystems;
  lib = nixpkgs.lib;
  pkgsToApps = attrs: lib.mapAttrs (_: pkg: { type = "app"; program = lib.getExe pkg; } ) attrs;

  crossPlatform = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      selfPkgs = self.packages.${system};
    in pkgsToApps {
      post-install = pkgs.callPackage ./post-install { inherit selfPkgs; };
      generate-oidc-client = pkgs.callPackage ./generate-oidc-client { };
    });

  linux = forLinuxSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      selfPkgs = self.packages.${system};
    in pkgsToApps {
      nixos-install = pkgs.callPackage ./nixos-install { inherit selfPkgs; };
    });

  darwin = forDarwinSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      selfPkgs = self.packages.${system};
    in pkgsToApps {
      darwin-install = pkgs.callPackage { inherit selfPkgs; };
    });
in mergeAllSystems [ crossPlatform linux darwin ]