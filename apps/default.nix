{ nixpkgs, generators, self, fleetFacts }:
let
  inherit (generators) forAllSystems forLinuxSystems mergeAllSystems;
  inherit (nixpkgs) lib;
  pkgsToApps = attrs: lib.mapAttrs (_: pkg: { type = "app"; program = lib.getExe pkg; meta = pkg.meta or {}; } ) attrs;

  crossPlatform = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      pkgsWithOverlays = import nixpkgs { inherit system; overlays = lib.attrValues self.overlays; };
      selfPkgs = self.packages.${system};
    in pkgsToApps {
      desktop-post-install = pkgs.callPackage ./desktop-post-install { inherit selfPkgs; };
      host-secrets = pkgs.callPackage ./host-secrets { inherit self; };
      fleet-json = pkgs.callPackage ./fleet-json { inherit fleetFacts; };
      check-updates = pkgsWithOverlays.callPackage ./check-updates {
        inherit (self.lib.builders.${system}) writeNushellScript;
      };
    });

  linux = forLinuxSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      selfPkgs = self.packages.${system};
    in pkgsToApps {
      nixos-install = pkgs.callPackage ./nixos-install { inherit selfPkgs; };
      seal-test = pkgs.callPackage ./seal-test { inherit self; };
    });
in mergeAllSystems [ crossPlatform linux ]
