{ nixpkgs, self, inputs }:
let
  inherit (nixpkgs.lib.attrsets) attrValues;
in
{
  # Lean guest builder: upstream NixOS modules + sops only (no home-manager/disko,
  # no selfhost-nix — guests use plain services and pull required modules
  # explicitly). Fleet settings (domain, smtp, …) live under compute, so inject
  # them into the guest's `private` for the same `private.settings.*` API.
  mkMicrovmGuest = { hostName, system ? "x86_64-linux", configPath }:
    nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = {
        inherit inputs;
        self = self // { lib = self.lib // { builders = self.lib.builders.${system}; }; };
        private = inputs.dotfiles-private.hosts.${hostName} // {
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
