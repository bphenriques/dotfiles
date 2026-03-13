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
      desktop-post-install = pkgs.callPackage ./desktop-post-install { inherit selfPkgs; };
      host-secrets = pkgs.callPackage ./host-secrets { };
      service-catalogue = pkgs.callPackage ./service-catalogue { };
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
      darwin-install = pkgs.callPackage ./darwin-install { inherit selfPkgs; };
    });
in mergeAllSystems [ crossPlatform linux darwin ]
