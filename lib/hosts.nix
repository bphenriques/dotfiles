{ nixpkgs, self, inputs }:
let
  inherit (nixpkgs.lib.attrsets) attrValues;
in
{
  # Lean builder for microvm guests: no home-manager (none of these
  # guests have interactive users). Imports sops-nix and exposes
  # `private = inputs.dotfiles-private.hosts.<hostName>` so secrets
  # follow the same convention as full hosts.
  #
  # Fleet-wide settings (domain, smtp, …) currently live under compute's
  # `private.settings`. Inject them into the guest's `private` so VM code
  # can read `private.settings.domain` directly instead of reaching across
  # to `inputs.dotfiles-private.hosts.compute.settings`. When dotfiles-
  # private grows a top-level fleet attrset, swap the source here.
  mkMicrovmGuest = { hostName, system, configPath }:
    nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = {
        inherit inputs self;
        private = (inputs.dotfiles-private.hosts.${hostName}) // {
          settings = inputs.dotfiles-private.hosts.compute.settings;
        };
      };
      modules = [
        inputs.sops-nix.nixosModules.sops
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
