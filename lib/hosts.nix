{ nixpkgs, self, inputs }:
let
  inherit (nixpkgs.lib.attrsets) attrValues;
  fleet = import ../hosts/shared.nix;   # imported once; threaded to guests via specialArgs
in
{
  mkMicrovmGuest = { hostName, system ? "x86_64-linux", configPath, placement }:
    nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = {
        inherit inputs placement fleet;   # placement allocated by the host; fleet = shared.nix, no path-imports in guests
        self = self // { lib = self.lib // { builders = self.lib.builders.${system}; }; };
        private = inputs.dotfiles-private.hosts.${hostName};
      };
      modules = [
        inputs.sops-nix.nixosModules.sops
        { nixpkgs.overlays = attrValues self.overlays; }
        { networking.hostName = nixpkgs.lib.mkDefault hostName; }
        configPath
      ];
    };

  mkNixosHost = { hostName, system ? "x86_64-linux", configPath, extraOverlays ? [], extraHmModules ? [] }:
    let
      sharedSpecialArgs = {
        inherit inputs;
        private = inputs.dotfiles-private.hosts.${hostName};
        self = self // {
          packages = self.packages.${system} // inputs.dotfiles-private.packages.${system};
          lib = self.lib // { builders = self.lib.builders.${system}; };
        };
      };
    in nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = sharedSpecialArgs;
      modules = attrValues self.nixosModules ++ [
        inputs.selfhost-nix.nixosModules.default
        inputs.sops-nix.nixosModules.sops
        inputs.disko.nixosModules.disko
        inputs.home-manager.nixosModules.home-manager
        { nixpkgs.overlays = attrValues self.overlays ++ extraOverlays; }
        {
          home-manager = {
            sharedModules = attrValues self.homeManagerModules ++ [
              inputs.nix-index-database.homeModules.nix-index
            ] ++ extraHmModules;
            extraSpecialArgs = sharedSpecialArgs;
          };
        }
        configPath
      ];
    };
}
