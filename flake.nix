{
  description = "bphenriques's Nix configuration for his machines";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs-unstable";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs-unstable";

    # Secret Manager
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs-unstable";

    # Automatically format disks using a declaractive specification
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs-unstable";

    # Other community flakes
    nur.url = "github:nix-community/nur";                         # Firefox extensions
    ghostty.url = "git+ssh://git@github.com/mitchellh/ghostty";   # Terminal
  };

  outputs = inputs @ { self, nixpkgs, nixpkgs-unstable, darwin, home-manager, sops-nix, disko, ... }:
    let
      inherit (nixpkgs.lib) attrValues;
      forAllSystems = nixpkgs.lib.genAttrs [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

      nixpkgsConfig = {
        overlays = (import ./overlays { inherit inputs; });
        config.allowUnfreePredicate = pkg: builtins.elem (nixpkgs.lib.getName pkg) [
          "discord"
          "nvidia-x11"
          "nvidia-settings"
          "nvidia-persistenced"
          "steam"
          "steam-original"
          "steam-run"
          "terraform"
          "keepa"
          "libretro-genesis-plus-gx"
          "libretro-snes9x"
          "libretro-fbneo"
          "onetab"

          # Fingerprint
          "libfprint-2-tod1-goodix-550a"

          # Cuda
          "libnvjitlink"
          "libnpp"
        ] || (nixpkgs.lib.strings.hasPrefix "cuda" (nixpkgs.lib.getName pkg)) || (nixpkgs.lib.strings.hasPrefix "libcu" (nixpkgs.lib.getName pkg)); # Cuda
        config.permittedInsecurePackages = [
          "electron-24.8.6"
          "electron-27.3.11"
          "electron-28.3.3"
        ];
      };

      # TODO: Should this moved to the top-level flakes.nix
      nixConfig = {
        settings = {
          experimental-features = [ "nix-command" "flakes" ]; # Enable nix flakes.
          auto-optimise-store   = true;                       # Optimise the store after each and every build (for the built path).
          use-xdg-base-directories = true;                    # Hide ~/.nix-profile and ~/.nix-defexpr

          extra-substituters = [
            "https://nix-community.cachix.org"
            "https://nixpkgs-wayland.cachix.org"
          ];
          extra-trusted-public-keys = [
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
            "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
          ];
        };

        # Ensure we have at least 5GiB always available in the drive. Less than that and my system gets unstable (need a new drive..).
        extraOptions = ''
          min-free = ${toString (5 * 1024 * 1024 * 1024)}
        '';
      };

      homeManagerModules = [
        sops-nix.homeManagerModules.sops
      ] ++ attrValues self.homeManagerModules;

      nixosLib = import ./lib/nixos.nix {
        inherit home-manager nixpkgsConfig homeManagerModules nixConfig;
        nixpkgs = nixpkgs-unstable;
        nixosModules = [
          sops-nix.nixosModules.sops
          disko.nixosModules.disko
        ] ++ attrValues self.nixosModules;
      };

      macosLib = import ./lib/macos.nix {
        inherit darwin home-manager nixConfig nixpkgsConfig homeManagerModules;
        darwinModules = attrValues self.darwinModules;
      };

      homeManagerLib = import ./lib/home-manager.nix {
        inherit home-manager nixpkgsConfig homeManagerModules;
        nixpkgs = nixpkgs-unstable;
      };
    in {
      apps = (import ./apps { inherit nixpkgs nixpkgs-unstable; });
      packages = import ./packages { inherit nixpkgs; };
      formatter = forAllSystems (system: inputs.nixpkgs.legacyPackages.${system}.nixfmt-rfc-style);
      devShells = forAllSystems (system: {
        default = (import ./shell.nix { pkgs = nixpkgs-unstable.legacyPackages.${system}; });
      });

      # Hosts
      nixosConfigurations = with nixosLib; {
        laptop = mkNixOSHost { hostModule = ./hosts/laptop; };
      };
      darwinConfigurations = with macosLib; {
        work-macos = mkMacOSHost { hostModule = ./hosts/work-macos; };
      };

      # Custom modules
      nixosModules        = import ./modules/nixos;
      homeManagerModules  = import ./modules/home-manager;
      darwinModules       = import ./modules/darwin;
    };
}
