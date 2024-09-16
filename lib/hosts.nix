{ lib, inputs, ... }:
let
  nixConfig = {
    settings = {
      experimental-features = [ "nix-command" "flakes" ]; # Enable nix flakes.
      auto-optimise-store   = true;                       # Optimise the store after each and every build (for the built path).
      use-xdg-base-directories = true;                    # Hide ~/.nix-profile and ~/.nix-defexpr
    };

    # Ensure we have at least 5GiB always available in the drive. Less than that and my system gets unstable (need a new drive..).
    extraOptions = ''
      min-free = ${toString (5 * 1024 * 1024 * 1024)}
    '';
  };

  nixpkgsConfig = {
    allowUnfree = true; # I was maintaining a list.. because it was _nicer_ and _explicit_ but.. I am lazy.
    permittedInsecurePackages = [ "electron-27.3.11" "electron-28.3.3" ];
  };

  mkExtraArgs = system: {
    self.pkgs = {
      dotfiles = inputs.self.packages.${system}.dotfiles;
      frg = inputs.self.packages.${system}.frg;
      ffd = inputs.self.packages.${system}.ffd;
      preview = inputs.self.packages.${system}.preview;

      fishPlugins = {
        dotfiles  = inputs.self.packages.${system}.dotfilesFishPlugin;
        ffd       = inputs.self.packages.${system}.ffdFishPlugin;
        frg       = inputs.self.packages.${system}.frgFishPlugin;
      };
    };
    community.pkgs = {
      ghostty = inputs.ghostty.packages.${system}.default;
    };
  };
in
{
  mkNixOSHost = { system ? "x86_64-linux", overlays, nixosModules, hmModules, hostModule }:
    let
      nixpkgs = inputs.nixpkgs-unstable;
      commonConfig = {
        nix = nixConfig;
        nixpkgs = {
          inherit overlays;
          config = nixpkgsConfig;
        };
        home-manager = {
          sharedModules = hmModules;
          extraSpecialArgs = mkExtraArgs system;
        };
      };
    in nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = mkExtraArgs system;
      modules = nixosModules ++ [ commonConfig hostModule ];
    };

  mkMacOSHost = { system ? "aarch64-darwin", overlays, darwinModules, hmModules, hostModule }:
    let
      nixpkgs = inputs.nixpkgs-unstable;
      commonConfig = {
        nix = nixConfig;
        nixpkgs = {
          inherit overlays;
          hostPlatform = system;
          config = nixpkgsConfig;
        };
        home-manager = {
          sharedModules = hmModules;
          extraSpecialArgs = mkExtraArgs system;
        };
      };
    in inputs.darwin.lib.darwinSystem {
      inherit system;
      specialArgs = mkExtraArgs system;
      modules = darwinModules ++ [ commonConfig hostModule ];
    };
}