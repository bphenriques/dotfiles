{
  description = "bphenriques's machines expressed using nix";

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
    # FIXME: pin to when NUR is available but I still have 6.10 kernel available. Replace the ref with "github:nixos/nixpkgs/nixpkgs-unstable"
    nixpkgs.url = "github:nixos/nixpkgs/942b12cabae1cd4414c7177472d759731de92cb4";        # Stable(ish) enough. Plus home-manager is _always_ on unstable.
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.11";      # I don't really use it, but leaving it here.

    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # Private dotfiles for confidential information that can't be covered by sops or want exposed
    dotfiles-private.url = "git+ssh://git@github.com/bphenriques/dotfiles-private";
    dotfiles-private.inputs.nixpkgs.follows = "nixpkgs";

    # Community flakes
    sops-nix.url = "github:Mic92/sops-nix";                       # Manage secrets using sops
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    disko.url = "github:nix-community/disko";                     # Declaratively describe my disks layout
    disko.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/nur";                         # Collection of packages. Use it for Firefox extensions
    ghostty.url = "git+ssh://git@github.com/mitchellh/ghostty";   # Terminal
    ghostty.inputs.nixpkgs-stable.follows = "nixpkgs";            # Fix OpenGL
    ghostty.inputs.nixpkgs-unstable.follows = "nixpkgs";          # Fix OpenGL
  };

  outputs = inputs @ { nixpkgs, ... }:
    let
      inherit (mylib.builders) forAllSystems;
      mylib = import ./lib { inherit inputs; lib = nixpkgs.lib; };
    in {
      apps      = import ./apps { inherit nixpkgs mylib; };
      packages  = import ./packages { inherit nixpkgs mylib; };
      formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.nixfmt-rfc-style);
      devShells = forAllSystems (system: {
        default = import ./shell.nix { pkgs = nixpkgs.legacyPackages.${system}; };
      });
      overlays      = import ./overlays { inherit inputs; };
      nixosModules  = import ./modules/nixos;

      # Hosts - Each host defines what it needs from the inputs.
      nixosConfigurations.laptop = import ./hosts/laptop { inherit mylib inputs; };
      darwinConfigurations.work-macos = import ./hosts/work-macos { inherit mylib inputs; };

      # Non standard flake outputs
      homeManagerModules  = import ./modules/home-manager;
      darwinModules       = import ./modules/darwin;
    };
}
