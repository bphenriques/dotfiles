{ inputs, mylib, ... }:
let
  inherit (inputs.nixpkgs.lib.attrsets) attrValues;

  darwinModules = attrValues inputs.self.darwinModules ++ [ inputs.home-manager.darwinModules.home-manager ];
  hmModules = attrValues inputs.self.homeManagerModules;
in mylib.hosts.mkMacOSHost {
  inherit darwinModules hmModules;
  hostModule = ./config.nix;
  extraSpecialArgs = {
    network-devices = import ../network-devices.nix;
  };
}
