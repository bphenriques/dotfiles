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

    # Other community flakes
    nur.url = "github:nix-community/nur";     # Mostly for Firefox extensions
    zjstatus.url = "github:dj95/zjstatus";    # ZelliJ plugin

    plasma-manager = {
      url = "github:pjones/plasma-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
  };

  outputs = inputs @ { self, nixpkgs, nixpkgs-unstable, darwin, home-manager, sops-nix, plasma-manager, ... }:
    let
      inherit (nixpkgs.lib) attrValues;
      nixpkgsConfig = {
        config.allowUnfreePredicate = pkg: builtins.elem (nixpkgs.lib.getName pkg) [
          "discord"
          "nvidia-x11"
          "nvidia-settings"
          "nvidia-persistenced"
          "steam"
          "steam-original"
          "steam-run"
          "unrar"
          "terraform"
          "keepa"
          "libretro-genesis-plus-gx"
          "libretro-snes9x"
          "libretro-fbneo"
        ];
        config.permittedInsecurePackages = [
          "electron-24.8.6" # FIXME: Unsure who uses it.
        ];
        overlays = (import ./overlays { inherit inputs; });
      };

      nixConfigNixOS = {
        optimise.automatic = true; # Sets up a systemd timer that regularly goes over all paths and optimises them
        gc = {
          automatic = true;
          dates = "weekly";
          options = "--delete-older-than 7d";
        };
      };

      nixConfig = {
        settings = {
          experimental-features = [ "nix-command" "flakes" ]; # Enable nix flakes.
          auto-optimise-store   = true;                       # Optimise the store after each and every build (for the built path).
        };

        # Ensure we have at least 5GiB always available in the drive. Less than that and my system gets unstable (need a new drive..).
        extraOptions = ''
          min-free = ${toString (5 * 1024 * 1024 * 1024)}
        '';
      };

      homeManagerModules = [
        sops-nix.homeManagerModules.sops
        plasma-manager.homeManagerModules.plasma-manager
      ] ++ attrValues self.homeManagerModules;

      nixosLib = import ./lib/nixos.nix {
        inherit home-manager nixpkgsConfig homeManagerModules;
        nixConfig = nixConfig // nixConfigNixOS;
        nixpkgs = nixpkgs-unstable;
        nixosModules = [ sops-nix.nixosModules.sops ] ++ attrValues self.nixosModules;
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
      # No alias is required: nixos-rebuild looks for the right configuration under nixosConfigurations by default.
      nixosConfigurations = with nixosLib; {
        desktop = mkNixOSHost { hostModule = ./host/desktop; };
      };

      darwinConfigurations = with macosLib; {
        work-macos = mkMacOSHost { hostModule = ./host/work-macos; };
      };

      # Custom modules. Either adds new feature or redefines functionality to have finer grain control over the output.
      nixosModules        = import ./nixos/modules;
      homeManagerModules  = import ./home/modules;
      darwinModules       = import ./darwin/modules;
    };
}
