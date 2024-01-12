{
  description = "bphenriques's Nix configuration for his machines";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";               # Default to stable for most things.
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable"; # Unstable for some packages.

    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs-unstable";             # Pin Darwin to unstable.

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs-unstable";       # Pin Home-Manager to unstable.

    # Other communicaty made repositories
    nur.url = "github:nix-community/nur";                           # Mostly for Firefox extensions
    zjstatus.url = "github:dj95/zjstatus";                          # ZelliJ plugin
  };

  outputs = inputs @ { self, nixpkgs, nixpkgs-unstable, darwin, home-manager, ... }:
    let
      nixpkgsConfig = {
        config.allowUnfreePredicate = pkg: builtins.elem (nixpkgs.lib.getName pkg) [
          "discord"
          "dropbox"
          "nvidia-x11"
          "nvidia-settings"
          "nvidia-persistenced"
          "steam"
          "steam-original"
          "steam-run"
          "plexmediaserver"
          "unrar"
          "terraform"
          "keepa"
        ];
        config.permittedInsecurePackages = [
          "electron-24.8.6" # Unsure who uses it.
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

      nixosLib = import ./lib/nixos.nix {
        inherit home-manager nixpkgsConfig;                                     # Modules and configurations.
        nixConfig = nixConfig // nixConfigNixOS;                                # NixOS specific configuration.
        nixpkgs = nixpkgs-unstable;                                             # Requires specific stage of nixpkgs.
        nixosModules = self.nixosModules;                                       # Custom nixos modules.
        homeManagerModules = self.homeManagerModules;                           # Custom home-manager modules.
        lib = nixpkgs-unstable.lib;                                             # Requires specific stage of nixpkgs.
      };

      macosLib = import ./lib/macos.nix {
        inherit darwin home-manager nixpkgsConfig nixConfig;  # Modules and configurations.
        darwinModules = self.darwinModules;                   # Custom darwin modules.
        homeManagerModules = self.homeManagerModules;         # Custom home-manager modules.
        lib = nixpkgs-unstable.lib;                           # Requires specific stage of nixpkgs.
      };

      homeManagerLib = import ./lib/home-manager.nix {
        inherit home-manager nixpkgsConfig;           # Modules and configurations.
        nixpkgs = nixpkgs-unstable;                   # Requires specific stage of nixpkgs.
        homeManagerModules = self.homeManagerModules; # Custom home-manager modules.
        lib = nixpkgs-unstable.lib;                   # Requires specific stage of nixpkgs.
      };
    in {
      # No alias is required: nixos-rebuild looks for the right configurating under nixosConfigurations by default.
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
