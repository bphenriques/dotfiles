{ nixpkgs, self, sops-nix, home-manager, dotfiles-private, nixos-raspberrypi, stylix, ... }:
let
  inherit (nixpkgs.lib.attrsets) attrValues;

  system = "aarch64-linux";

  nixModule = {
    nixpkgs = {
      overlays = attrValues self.overlays;
    };
  };

  # Only include modules needed for inky (not all homelab services)
  nixosModules = [
    self.nixosModules.homelab-cifs
    self.nixosModules.homelab-paths
    sops-nix.nixosModules.sops
    home-manager.nixosModules.home-manager
  ];

  sharedSpecialArgs = {
    inherit nixos-raspberrypi;
    self = self // {
      pkgs = self.packages.${system} // dotfiles-private.packages.${system};
      lib.builders = self.lib.builders.${system};
      private = dotfiles-private;
      shared = import ../shared.nix;
    };
  };

  hmModule = {
    home-manager = {
      sharedModules = attrValues self.homeManagerModules ++ [
        stylix.homeModules.stylix
      ];
      extraSpecialArgs = sharedSpecialArgs;
    };
  };

  rpiModules = with nixos-raspberrypi.nixosModules; [
    raspberry-pi-02.base
  ];
in nixos-raspberrypi.lib.nixosSystem {
  specialArgs = sharedSpecialArgs;
  modules = nixosModules ++ rpiModules ++ [ nixModule hmModule ./config.nix ];
}
