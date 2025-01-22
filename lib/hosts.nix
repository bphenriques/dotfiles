{ lib, inputs, ... }:
let
  nixConfig = {
    optimise.automatic = true;
    settings = {
      experimental-features = [ "nix-command" "flakes" ]; # Enable nix flakes.
      use-xdg-base-directories = true;                    # Hide ~/.nix-profile and ~/.nix-defexpr
      warn-dirty = false;                                 # I know...
    };

    # Ensure we have at least 5GiB always available. Less than that and my system gets unstable.
    extraOptions = "min-free = ${toString (5 * 1024 * 1024 * 1024)}";
  };

  nixpkgsConfig = {
    allowUnfree = true; # I was maintaining a list.. because it was _nicer_ and _explicit_ but.. I am lazy.
    permittedInsecurePackages = [ "electron-27.3.11" "electron-28.3.3" "electron-31.7.7" ];
  };
in
{
  mkNixOSHost = { system, overlays ? [ ], nixosModules, hmModules, hostModule, nixosSpecialArgs ? { }, hmSpecialArgs ? { } }:
    let
      specialArgs = nixosSpecialArgs;
      commonConfig = {
        nix = nixConfig;
        nixpkgs = {
          inherit overlays;
          config = nixpkgsConfig;
        };
        home-manager = {
          sharedModules = hmModules;
          extraSpecialArgs = hmSpecialArgs;
        };
      };
    in inputs.nixpkgs.lib.nixosSystem {
      inherit system specialArgs;
      modules = nixosModules ++ [ commonConfig hostModule ];
    };

  mkMacOSHost = { system ? "aarch64-darwin", overlays ? [ ], darwinModules, hmModules, hostModule, darwinSpecialArgs ? { }, hmSpecialArgs ? { } }:
    let
      specialArgs = darwinSpecialArgs;
      commonConfig = {
        nix = nixConfig;
        nixpkgs = {
          inherit overlays;
          hostPlatform = system;
          config = nixpkgsConfig;
        };
        home-manager = {
          sharedModules = hmModules;
          extraSpecialArgs = hmSpecialArgs;
        };
      };
    in inputs.darwin.lib.darwinSystem {
      inherit system specialArgs;
      modules = darwinModules ++ [ commonConfig hostModule ];
    };
}
