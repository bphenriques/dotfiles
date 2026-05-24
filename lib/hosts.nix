{ nixpkgs, self, inputs }:
let
  inherit (nixpkgs.lib.attrsets) attrValues;
in
{
  # Lean builder for microvm guests: no home-manager, no
  # dotfiles-private dependency, just nixpkgs + our overlays + the
  # guest config. The guest typically imports
  # `inputs.microvm.nixosModules.microvm` and any service modules it
  # needs directly.
  mkMicrovmGuest = { hostName, system, configPath }:
    nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = { inherit inputs self; };
      modules = [
        { nixpkgs.overlays = attrValues self.overlays; }
        { networking.hostName = nixpkgs.lib.mkDefault hostName; }
        configPath
      ];
    };

  mkNixosHost = { hostName, system, configPath, extraOverlays ? [], extraHmModules ? [] }:
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
        inputs.sops-nix.nixosModules.sops
        inputs.disko.nixosModules.disko
        inputs.home-manager.nixosModules.home-manager
        { nixpkgs.overlays = attrValues self.overlays ++ extraOverlays; }
        {
          home-manager = {
            sharedModules = attrValues self.homeManagerModules ++ [
              inputs.nix-index-database.hmModules.nix-index
            ] ++ extraHmModules;
            extraSpecialArgs = sharedSpecialArgs;
          };
        }
        configPath
      ];
    };
}
