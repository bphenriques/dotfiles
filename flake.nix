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

    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # Private dotfiles for confidential information required in build-time
    dotfiles-private.url = "git+ssh://git@github.com/bphenriques/dotfiles-private";
    dotfiles-private.inputs.nixpkgs.follows = "nixpkgs";

    # Community flakes
    stylix.url = "github:danth/stylix";           # Consistent coloring across my system. I can still tweak manually.
    nur.url = "github:nix-community/nur";         # Collection of packages. Use it for Firefox extensions
    sops-nix.url = "github:Mic92/sops-nix";       # Manage secrets using sops
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    disko.url = "github:nix-community/disko";     # Declaratively describe my disks layout
    disko.inputs.nixpkgs.follows = "nixpkgs";
    treefmt-nix.url = "github:numtide/treefmt-nix"; # Unified formatter for multiple languages
  };

  outputs = inputs @ { self, nixpkgs, treefmt-nix, ... }:
    let
      generators = import ./lib/generators.nix { lib = nixpkgs.lib; };
      inherit (generators) forAllSystems readModulesAttrs;
      treefmtEval = forAllSystems (system: treefmt-nix.lib.evalModule nixpkgs.legacyPackages.${system} ./treefmt.nix);
      inherit (import ./lib/hosts.nix { inherit nixpkgs self inputs; }) mkNixosHost;
    in {
      lib.builders = forAllSystems (system:
        import ./lib/builders.nix {
          lib = nixpkgs.lib;
          pkgs = nixpkgs.legacyPackages.${system};
        }
      );
      apps      = import ./apps { inherit nixpkgs self generators; };
      packages  = import ./packages { inherit nixpkgs generators; };
      formatter = forAllSystems (system: treefmtEval.${system}.config.build.wrapper); # `nix fmt`
      checks    = forAllSystems (system: {                                            # `nix flake check`
        formatting = treefmtEval.${system}.config.build.check self;
      } // nixpkgs.lib.optionalAttrs (system == "x86_64-linux") {
        eval-compute = self.nixosConfigurations.compute.config.system.build.toplevel;
        eval-laptop = self.nixosConfigurations.laptop.config.system.build.toplevel;
      });
      devShells = forAllSystems (system: {
        default = import ./shell.nix { pkgs = nixpkgs.legacyPackages.${system}; };
      });
      overlays  = import ./overlays inputs;

      # Modules
      nixosModules        = readModulesAttrs ./modules/nixos;
      homeManagerModules  = readModulesAttrs ./modules/home-manager;
      darwinModules       = readModulesAttrs ./modules/darwin;

      nixosConfigurations.compute = mkNixosHost {
        system = "x86_64-linux";
        configPath = ./hosts/compute;
      };
      nixosConfigurations.laptop = mkNixosHost {
        system = "x86_64-linux";
        configPath = ./hosts/laptop;
        extraOverlays = [ inputs.nur.overlays.default ];
        extraHmModules = [ inputs.stylix.homeModules.stylix ];
      };
    };
}
