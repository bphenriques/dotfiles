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

    impermanence.url = "github:nix-community/impermanence/d3523715d7cf654cda66176ea71413db159f5b71";       # Automatically clean-up unneded files on boot
    #impermanence.url = "github:nix-community/impermanence";       # Automatically clean-up unneded files on boot
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
        plasma-manager.homeManagerModules.plasma-manager
        impermanence.nixosModules.home-manager.impermanence
      ] ++ attrValues self.homeManagerModules;

      nixosLib = import ./lib/nixos.nix {
        inherit home-manager nixpkgsConfig homeManagerModules nixConfig;
        nixpkgs = nixpkgs-unstable;
        nixosModules = [
          sops-nix.nixosModules.sops
          disko.nixosModules.disko
          impermanence.nixosModules.impermanence
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
      apps = (import ./apps { inherit nixpkgs; });

      # Hosts
      nixosConfigurations = with nixosLib; {
        desktop = mkNixOSHost { hostModule = ./hosts/desktop; };
        laptop = mkNixOSHost { hostModule = ./hosts/laptop; };
        minimal = mkNixOSHost { hostModule = ./hosts/minimal; };
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
