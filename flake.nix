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
    zjstatus.url = "github:dj95/zjstatus";                        # Terminal's multiplexer plugin
    impermanence.url = "github:nix-community/impermanence";       # Automatically clean-up unneded files on boot
    ghostty.url = "git+ssh://git@github.com/mitchellh/ghostty";   # Terminal
    plasma-manager = {                                            # Desktop environment
      url = "github:pjones/plasma-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
  };

  outputs = inputs @ { self, nixpkgs, nixpkgs-unstable, darwin, home-manager, sops-nix, disko, plasma-manager, impermanence, ... }:
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
          "onetab"

          # Cuda
          "libnvjitlink"
          "libnpp"
        ] || (nixpkgs.lib.strings.hasPrefix "cuda" (nixpkgs.lib.getName pkg)) || (nixpkgs.lib.strings.hasPrefix "libcu" (nixpkgs.lib.getName pkg)); # Cuda
        config.permittedInsecurePackages = [
          "electron-24.8.6"
          "electron-27.3.11"
          "electron-28.3.3"
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
        settings = {
          auto-optimise-store = true;
          # TODO: Consider this for wayland: https://github.com/jordanisaacs/dotfiles/blob/master/modules/system/core/default.nix#L92
        };
      };

      nixConfig = {
        settings = {
          experimental-features = [ "nix-command" "flakes" ]; # Enable nix flakes.
          auto-optimise-store   = true;                       # Optimise the store after each and every build (for the built path).
          #use-xdg-base-directories = true;                    # Hide ~/.nix-profile and ~/.nix-defexpr
        };

        # Ensure we have at least 5GiB always available in the drive. Less than that and my system gets unstable (need a new drive..).
        extraOptions = ''
          min-free = ${toString (5 * 1024 * 1024 * 1024)}
        '';
      };

      homeManagerModules = [
        sops-nix.homeManagerModules.sops
        plasma-manager.homeManagerModules.plasma-manager
        impermanence.nixosModules.home-manager.impermanence
      ] ++ attrValues self.homeManagerModules;

      nixosModules = [
        sops-nix.nixosModules.sops
        disko.nixosModules.disko
        impermanence.nixosModules.impermanence
      ] ++ attrValues self.nixosModules;

      nixosLib = import ./lib/nixos.nix {
        inherit home-manager nixpkgsConfig homeManagerModules nixosModules;
        nixConfig = nixConfig // nixConfigNixOS;
        nixpkgs = nixpkgs-unstable;
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
      apps = (import ./apps { inherit nixpkgs; });

      # Hosts
      nixosConfigurations = with nixosLib; {
        desktop = mkNixOSHost { hostModule = ./hosts/desktop; };
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
