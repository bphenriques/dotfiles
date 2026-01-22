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
  };

  outputs = inputs @ { self, nixpkgs, ... }:
    let
      generators = import ./lib/generators.nix { lib = nixpkgs.lib; };
      inherit (generators) forAllSystems readModulesAttrs;
    in {
      apps      = import ./apps { inherit nixpkgs self generators; };
      packages  = import ./packages { inherit nixpkgs generators; };
      formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.nixfmt-rfc-style);
      devShells = forAllSystems (system: {
        default = import ./shell.nix { pkgs = nixpkgs.legacyPackages.${system}; };
      });
      overlays  = import ./overlays inputs;

      # Modules
      nixosModules        = readModulesAttrs ./modules/nixos;
      homeManagerModules  = readModulesAttrs ./modules/home-manager;
      darwinModules       = readModulesAttrs ./modules/darwin;

      # Hosts
      nixosConfigurations.laptop = import ./hosts/laptop inputs;
      nixosConfigurations.compute = import ./hosts/compute inputs;
      darwinConfigurations.work-macos = import ./hosts/work-macos inputs;
    };
}
