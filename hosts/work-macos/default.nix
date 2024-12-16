{ inputs, mylib, ... }:
let
  inherit (inputs.nixpkgs.lib.attrsets) attrValues;

  darwinModules = attrValues inputs.self.darwinModules ++ [ inputs.home-manager.darwinModules.home-manager ];
  hmModules = attrValues inputs.self.homeManagerModules;
  specialArgs = {
    self = {
      pkgs = inputs.self.packages.${system};
      private = inputs.dotfiles-private.packages.${system};
    };
    network-devices = import ../network-devices.nix;
  };
in mylib.hosts.mkMacOSHost {
  inherit darwinModules hmModules;
  hostModule = ./config.nix;
  darwinSpecialArgs = specialArgs;
  hmSpecialArgs    = specialArgs;
}
