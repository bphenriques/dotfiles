{ nixpkgs, self, inputs }:
let
  inherit (nixpkgs.lib.attrsets) attrValues;
in
{
  mkNixosHost = { hostName, system, configPath, extraOverlays ? [], extraHmModules ? [] }:
    let
      sharedSpecialArgs = {
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
