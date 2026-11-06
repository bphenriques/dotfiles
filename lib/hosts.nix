{ nixpkgs, self, inputs }:
let
  inherit (nixpkgs.lib.attrsets) attrValues;
  fleet = import ../hosts/shared.nix;

  mkSpecialArgs = { hostName, system }: {
    inherit inputs;
    private = inputs.dotfiles-private.hosts.${hostName};
    self = self // {
      packages = self.packages.${system} // inputs.dotfiles-private.packages.${system};
      lib = self.lib // { builders = self.lib.builders.${system}; };
    };
  };
in
{
  mkMicrovmGuest = { hostName, system ? "x86_64-linux", configPath, guestPlacement, fleetFacts ? { } }:
    nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = mkSpecialArgs { inherit hostName system; } // { inherit fleetFacts; };
      modules = [
        self.nixosModules.microvm-guest
        self.nixosModules.fleet-shared
        { my.microvm.guest = guestPlacement; }   # allocated by the host
        { nixpkgs.overlays = attrValues self.overlays; }
        { networking.hostName = hostName; }
        { inherit fleet; }
        configPath
      ];
    };

  mkNixosHost = { hostName, system ? "x86_64-linux", configPath, extraOverlays ? [] }:
    nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = mkSpecialArgs { inherit hostName system; };
      # microvm-guest turns whatever imports it into a guest, so only mkMicrovmGuest takes it.
      modules = attrValues (removeAttrs self.nixosModules [ "microvm-guest" ]) ++ [
        { nixpkgs.overlays = attrValues self.overlays ++ extraOverlays; }
        { networking.hostName = hostName; }
        { inherit fleet; }
        configPath
      ];
    };

  mkDarwinHost = { hostName, system ? "aarch64-darwin", configPath }:
    inputs.nix-darwin.lib.darwinSystem {
      specialArgs = mkSpecialArgs { inherit hostName system; };
      modules = [
        self.nixosModules.fleet-shared
        { nixpkgs.hostPlatform = system; }
        { inherit fleet; }
        { nixpkgs.overlays = attrValues self.overlays; }
        configPath
      ];
    };
}
