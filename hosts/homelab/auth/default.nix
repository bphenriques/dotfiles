{ nixpkgs, self, sops-nix, microvm, dotfiles-private, ... }:
let
  inherit (nixpkgs.lib.attrsets) attrValues;

  system = "x86_64-linux";

  nixModule = {
    nixpkgs = {
      overlays = attrValues self.overlays;
    };
  };

  nixosModules = [
    sops-nix.nixosModules.sops
    microvm.nixosModules.microvm
  ];

  sharedSpecialArgs = {
    self = self // {
      pkgs = self.packages.${system};
      lib.builders = self.lib.builders.${system};
      settings = dotfiles-private.settings;
    };
  };
in nixpkgs.lib.nixosSystem {
  inherit system;
  specialArgs = sharedSpecialArgs;
  modules = nixosModules ++ [ nixModule ./config.nix ];
}
