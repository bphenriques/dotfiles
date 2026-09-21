{ nixpkgs, self, inputs }:
let
  inherit (nixpkgs.lib.attrsets) attrValues;
  fleet = import ../hosts/shared.nix;
in
{
  mkMicrovmGuest = { hostName, system ? "x86_64-linux", configPath, guestPlacement, fleetFacts ? { } }:
    nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = {
        inherit inputs fleet fleetFacts;   # fleet = shared.nix, no path-imports in guests
        self = self // { lib = self.lib // { builders = self.lib.builders.${system}; }; };
        private = inputs.dotfiles-private.hosts.${hostName};
      };
      modules = [
        self.nixosModules.microvm-guest
        { custom.microvm.guest = guestPlacement; }   # allocated by the host
        { nixpkgs.overlays = attrValues self.overlays; }
        { networking.hostName = hostName; }
        configPath
      ];
    };

  mkNixosHost = { hostName, system ? "x86_64-linux", configPath, extraOverlays ? [] }:
    nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = {
        inherit inputs fleet;
        private = inputs.dotfiles-private.hosts.${hostName};
        self = self // {
          packages = self.packages.${system} // inputs.dotfiles-private.packages.${system};
          lib = self.lib // { builders = self.lib.builders.${system}; };
        };
      };
      # microvm-guest turns whatever imports it into a guest, so only mkMicrovmGuest takes it.
      modules = attrValues (removeAttrs self.nixosModules [ "microvm-guest" ]) ++ [
        { nixpkgs.overlays = attrValues self.overlays ++ extraOverlays; }
        { networking.hostName = hostName; }
        configPath
      ];
    };
}
