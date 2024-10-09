{ self, mylib, nixpkgs, home-manager, ... }:
let
  inherit (nixpkgs.lib.attrsets) attrValues;

  darwinModules = attrValues self.darwinModules ++ [ home-manager.darwinModules.home-manager ];
  hmModules = attrValues self.homeManagerModules;
in mylib.hosts.mkMacOSHost {
  inherit darwinModules hmModules;
  hostModule = ./config.nix;
  extraSpecialArgs = {
    network-devices = import ../network-devices.nix;
  };
}
