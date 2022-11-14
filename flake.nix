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

  outputs = inputs @ { self, nixpkgs, darwin, home-manager, ... }:
    let
      inherit (inputs.nixpkgs-unstable.lib) attrValues;

      nixpkgsConfig = {
        config = { allowUnfree = true; };
      };

      nixConfig = {
        settings = {
          experimental-features = [ "nix-command" "flakes" ]; # Enable nix flakes.
          auto-optimise-store   = true;                       # Ensure /nix/store does not grow eternally.
        };
      };

      mkMacOSHost = { system ? "aarch64-darwin", username, hostDarwinModules ? [], hostHomeManagerModules ? []}:
        let
          common = {
            nixpkgs = nixpkgsConfig;
            nix = nixConfig;

            # Nix Darwin
            services.nix-daemon.enable      = true;                 # Using nix-daemon (the only supported way).
            users.users."${username}".home  = "/Users/${username}"; # Set user's home.
            imports                         = [./macos/common.nix]; # Import common settings

            # Home-Manager
            home-manager.useGlobalPkgs        = true; # Consistency: use pkgs set via the system level nixpkgs options.
            home-manager.useUserPackages      = true; # Install packages defined in home-manager.
            home-manager.users."${username}"  = {
              imports = [./home/common.nix] ++ attrValues self.homeManagerModules;
            };

            system.stateVersion = 4;                                # Nix-Darwin config version.
          };

          host = {
            imports = hostDarwinModules;
            home-manager.users."${username}" = {
              imports = hostHomeManagerModules;
            };
          };
        in darwin.lib.darwinSystem {
          inherit system;
          modules = [home-manager.darwinModules.home-manager common host] ++ attrValues self.darwinModules;
          inputs = { inherit username; };
        };

      mkHomeManagerHost = { system, username, hostModules ? [] }:
        let
          common = {
            # TODO: Cant put more settings here.. Likely makes sense as this only manages my home?
            # nix = nixConfig;
            # nix.settings = nixConfig.settings;
            home = {
              inherit username;
              homeDirectory = "/home/${username}";
            };
            imports = [./home/common.nix];
          };
        in
          home-manager.lib.homeManagerConfiguration {
            pkgs = import inputs.nixpkgs-unstable {
              inherit system;
              inherit (nixpkgsConfig) config;
            };
            modules = [common] ++ attrValues self.homeManagerModules ++ hostModules;
          };
    in
    {
      darwinConfigurations = {
        work-macos = mkMacOSHost {
          username                = "brunohenriques";
          hostDarwinModules       = [./macos/work.nix];
          hostHomeManagerModules  = [./home/work.nix];
        };
      };

      homeManagerConfigurations = {
        wsl = mkHomeManagerHost {
          system    = "x86_64-linux";
          username  = "bphenriques";
        };
      };

      # Handy aliases
      work-macos     = self.darwinConfigurations.work-macos.system;
      wsl            = self.homeManagerConfigurations.wsl.activationPackage;

      # Custom modules. Either adds functionality or redefines in order to finer grain control over the output.
      homeManagerModules = {
        bphenriques-zsh           = ./modules/home-manager/zsh.nix;
        bphenriques-fzf-extra     = ./modules/home-manager/fzf-extra.nix;
        bphenriques-thefuck       = ./modules/home-manager/thefuck.nix;
        bphenriques-direnv-extra  = ./modules/home-manager/direnv-extra.nix;
        bphenriques-powerlevel10k = ./modules/home-manager/powerlevel10k.nix;
      };

      darwinModules = {
        org-protocol = ./modules/darwin/org-protocol;
        system-screencapture = ./modules/darwin/system/screencapture;
      };
    };
}
