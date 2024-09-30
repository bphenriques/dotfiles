{ self, mylib, nixpkgs, darwinModules, home-manager, ... }:
let
  inherit (nixpkgs.lib.attrsets) attrValues;

  overlays = attrValues self.overlays ++ [ nur.overlay ];
  darwinModules = attrValues self.darwinModules ++ [ home-manager.darwinModules.home-manager ];
  hmModules = attrValues self.homeManagerModules;
in mylib.hosts.mkMacOSHost {
  inherit darwinModules hmModules overlays;
  hostModule = ./config.nix;
}
