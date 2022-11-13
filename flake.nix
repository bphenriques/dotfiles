{
  description = "bphenriques's Nix configuration for his machines";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";               # Default to stable for most things.
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable"; # Unstable for some packages.

    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs-unstable";             # Pin Darwin to unstable.

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs-unstable";       # Pin Home-Manager to unstable.
  };

  outputs = { self, nixpkgs, darwin, home-manager, ... }@inputs:
    let
      nixpkgsConfig = {
        config = { allowUnfree = true; };
      };

      nixConfig = {
        settings = {
          experimental-features = [ "nix-command" "flakes" ]; # Enable nix flakes.
          auto-optimise-store   = true;                       # Ensure /nix/store does not grow eternally.
        };
      };

      mkMacOSHost = hostModule:
        let
          commonDarwinModule = {
            nixpkgs = nixpkgsConfig;
            nix = nixConfig;

            # Nix Darwin
            services.nix-daemon.enable = true;      # Using nix-daemon (the only supported way).
            system.stateVersion        = 4;         # Nix-Darwin config version.

            # Home-Manager
            home-manager.useGlobalPkgs    = true;    # For consistency, use global pkgs configured via the system level nixpkgs options.
            home-manager.useUserPackages  = true;    # Install packages defined in home-manager.
         };
        in darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          modules = [
            home-manager.darwinModules.home-manager
            commonDarwinModule
            hostModule
          ];
        };

      mkHomeManagerHost = { system ? "x86_64-linux", username, hostModule }:
        let
          baseModule = {
            # nix = nixConfig;
            home = {
              inherit username;
              homeDirectory = "/home/${username}";
            };
          };
        in
          home-manager.lib.homeManagerConfiguration {
            pkgs = import nixpkgs {
              inherit system;
              inherit (nixpkgsConfig) config;
            };
            modules = [ baseModule hostModule ];
          };
    in
    {
      darwinConfigurations = {
        work-macos = mkMacOSHost ./hosts/work-macos.nix;
      };

      homeManagerConfigurations = {
        wsl = mkHomeManagerHost {
          username    = "bphenriques";
          hostModule  = ./hosts/wsl.nix;
        };
      };

      # Handy aliases
      work-macos     = self.darwinConfigurations.work-macos.system;
      wsl            = self.homeManagerConfigurations.wsl.activationPackage;
    };
}
