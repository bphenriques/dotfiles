{
  description = "bphenriques's fleet";
  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://nixpkgs-wayland.cachix.org"
   ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
   ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";        # Stable(ish) enough. Plus home-manager is _always_ on unstable.

    # Personal flakes
    dotfiles-private.url = "git+ssh://git@github.com/bphenriques/dotfiles-private"; # Private dotfiles
    dotfiles-private.inputs.nixpkgs.follows = "nixpkgs";
    selfhost-nix.url = "git+ssh://git@github.com/bphenriques/selfhost-nix";         # Selfhost abstractions
    selfhost-nix.inputs.nixpkgs.follows = "nixpkgs";

    # Community flakes
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";                                 # Consistent coloring across my system. I can still tweak manually.
    nur.url = "github:nix-community/nur";                               # Collection of packages. Use it for Firefox extensions
    sops-nix.url = "github:Mic92/sops-nix";                             # Manage secrets using sops
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    disko.url = "github:nix-community/disko";                           # Declaratively describe my disks layout
    disko.inputs.nixpkgs.follows = "nixpkgs";
    treefmt-nix.url = "github:numtide/treefmt-nix";                     # Unified formatter for multiple languages
    nix-index-database.url = "github:nix-community/nix-index-database"; # Pre-built nix-index database for comma
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
    microvm.url = "github:microvm-nix/microvm.nix";                     # Lightweight, isolated guests
    microvm.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ { self, nixpkgs, treefmt-nix, ... }:
    let
      generators = import ./lib/generators.nix { inherit (nixpkgs) lib; };
      hostBuilders = import ./lib/hosts.nix { inherit nixpkgs self inputs; };
      inherit (generators) forAllSystems readModulesAttrs;
      inherit (hostBuilders) mkNixosHost mkMicrovmGuest;

      # One eval-<name> check per host/guest on `system`, derived — no hardcoded list to drift.
      evalChecks = system: nixpkgs.lib.mapAttrs'
        (name: cfg: nixpkgs.lib.nameValuePair "eval-${name}" cfg.config.system.build.toplevel)
        (nixpkgs.lib.filterAttrs (_: cfg: cfg.pkgs.stdenv.hostPlatform.system == system) self.nixosConfigurations);
      treefmtEval = forAllSystems (system: treefmt-nix.lib.evalModule nixpkgs.legacyPackages.${system} ./treefmt.nix);
    in {
      lib.builders = forAllSystems (system:
        import ./lib/builders.nix {
          inherit (nixpkgs) lib;
          pkgs = nixpkgs.legacyPackages.${system};
        }
      );
      apps      = import ./apps { inherit nixpkgs self generators; };
      packages  = import ./packages { inherit nixpkgs generators; builders = self.lib.builders; };
      overlays  = import ./overlays inputs;
      checks    = forAllSystems (system: {
        formatting = treefmtEval.${system}.config.build.check self;
      } // evalChecks system);
      formatter = forAllSystems (system: treefmtEval.${system}.config.build.wrapper);
      devShells = forAllSystems (system: {
        default = import ./shell.nix { pkgs = nixpkgs.legacyPackages.${system}; };
      });

      # Modules
      nixosModules        = readModulesAttrs ./modules/nixos;
      homeManagerModules  = readModulesAttrs ./modules/home-manager;

      # Hosts
      nixosConfigurations = let
        computeMicrovm = import ./hosts/compute/microvm/guests.nix;
        fleetFacts = import ./lib/fleet.nix {
          inherit (self) nixosConfigurations;
          inherit (nixpkgs) lib;
          producers = [ "compute" ];   # selfhost hosts that supply landing-page services
        };
        microvmGuests = nixpkgs.lib.mapAttrs (name: entry: mkMicrovmGuest {
          hostName = name;
          configPath = ./hosts/guests/${name};
          inherit fleetFacts;
          guestPlacement = {
            inherit (entry) ip mac vsockCid;
            inherit (computeMicrovm.bridge) gateway prefixLength;
          };
        }) computeMicrovm.guests;
      in {
        laptop = mkNixosHost {
          hostName = "laptop";
          configPath = ./hosts/laptop;
          extraOverlays = [ inputs.nur.overlays.default ];
        };
        compute = mkNixosHost {
          hostName = "compute";
          configPath = ./hosts/compute;
        };
      } // microvmGuests;
    };
}
