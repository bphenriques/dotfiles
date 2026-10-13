{ self, nixpkgs, home-manager, nix-darwin, dotfiles-private, stylix, ... }:
let
  inherit (nixpkgs.lib.attrsets) attrValues;

  system = "aarch64-darwin";
  nixModule = {
    nixpkgs = {
      hostPlatform = system;
      overlays = [ ];
    };
  };

  darwinModules = [ home-manager.darwinModules.home-manager ];

  hmModule = {
    home-manager = {
      sharedModules = attrValues self.homeManagerModules;
      extraSpecialArgs = { };
    };
  };
in nix-darwin.lib.darwinSystem {
  inherit system;
  modules = darwinModules ++ [ nixModule hmModule ./config.nix ];
}
