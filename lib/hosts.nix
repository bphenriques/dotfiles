{ lib, inputs, ... }:
let
  nixConfig = {
    settings = {
      experimental-features = [ "nix-command" "flakes" ]; # Enable nix flakes.
      auto-optimise-store   = true;                       # Optimise the store when building.
      use-xdg-base-directories = true;                    # Hide ~/.nix-profile and ~/.nix-defexpr
      warn-dirty = false;                                 # I know...
    };

    # Ensure we have at least 5GiB always available. Less than that and my system gets unstable.
    extraOptions = "min-free = ${toString (5 * 1024 * 1024 * 1024)}";
  };

  nixpkgsConfig = {
    allowUnfree = true; # I was maintaining a list.. because it was _nicer_ and _explicit_ but.. I am lazy.
    permittedInsecurePackages = [ "electron-27.3.11" "electron-28.3.3" ];
  };

  # TODO: This should not be abstracted the way it is (likely).
  mkExtraArgs = system: extraSpecialArgs: {
    self = {
      pkgs = inputs.self.packages.${system};
      private = inputs.dotfiles-private.packages.${system};
    };
    community.pkgs = {
      ghostty = inputs.ghostty.packages.${system}.default;
      firefox-addons = inputs.firefox-addons.packages.${system};
    };

    host = { }; # Intentionally empty, each host sets as required. This just ensures the root config 'host' is available.
  } // extraSpecialArgs;
in
{
  mkNixOSHost = { system ? "x86_64-linux", overlays ? [ ], nixosModules, hmModules, hostModule, extraSpecialArgs ? { } }:
    let
      specialArgs = (mkExtraArgs system extraSpecialArgs);
      commonConfig = {
        nix = nixConfig;
        nixpkgs = {
          inherit overlays;
          config = nixpkgsConfig;
        };
        home-manager = {
          sharedModules = hmModules;
          extraSpecialArgs = specialArgs;
        };
      };
    in inputs.nixpkgs.lib.nixosSystem {
      inherit system specialArgs;
      modules = nixosModules ++ [ commonConfig hostModule ];
    };

  mkMacOSHost = { system ? "aarch64-darwin", overlays ? [ ], darwinModules, hmModules, hostModule, extraSpecialArgs ? { } }:
    let
      specialArgs = (mkExtraArgs system extraSpecialArgs);
      commonConfig = {
        nix = nixConfig;
        nixpkgs = {
          inherit overlays;
          hostPlatform = system;
          config = nixpkgsConfig;
        };
        home-manager = {
          sharedModules = hmModules;
          extraSpecialArgs = specialArgs;
        };
      };
    in inputs.darwin.lib.darwinSystem {
      inherit system specialArgs;
      modules = darwinModules ++ [ commonConfig hostModule ];
    };
}
