{ lib, inputs, ... }:
let
  sharedNixConfig = {
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
in
{
  mkNixOSHost = { system ? "x86_64-linux", hostConfig, nixpkgsConfig, specialArgs ? {} }:
    let
      nixpkgs = inputs.nixpkgs-unstable;
      lib = nixpkgs.lib;

      homeManagerModules = [
        inputs.sops-nix.homeManagerModules.sops
      ] ++ (lib.attrsets.attrValues inputs.self.homeManagerModules);

      commonConfig = {
        nixpkgs = nixpkgsConfig;
        nix = sharedNixConfig;

        home-manager.useGlobalPkgs    = true;               # Use pkgs set within nixpkgs.
        home-manager.useUserPackages  = true;               # Install packages defined in home-manager.
        home-manager.sharedModules    = homeManagerModules; # Custom modules.
      };
      commonModules = [
        inputs.sops-nix.nixosModules.sops
        inputs.disko.nixosModules.disko
        inputs.home-manager.nixosModules.home-manager
      ] ++ lib.attrsets.attrValues inputs.self.nixosModules;
    in nixpkgs.lib.nixosSystem {
      inherit system specialArgs;
      modules = commonModules ++ [ commonConfig hostConfig ];
    };

  mkMacOSHost = { system ? "aarch64-darwin", hostConfig, nixpkgsConfig }:
    let
      nixpkgs = inputs.nixpkgs-unstable;
      lib = nixpkgs.lib;

      homeManagerModules = [
        inputs.sops-nix.homeManagerModules.sops
      ] ++ (lib.attrsets.attrValues inputs.self.homeManagerModules);

      darwinModules = [
        inputs.home-manager.darwinModules.home-manager
      ] ++ (lib.attrsets.attrValues inputs.self.darwinModules);

      commonConfig = {
        nixpkgs = nixpkgsConfig // {
          hostPlatform = system;
        };
        nix = sharedNixConfig;

        # Nix Darwin
        services.nix-daemon.enable   = true;                # Using nix-daemon (the only supported way).

        # Home-Manager
        home-manager.useGlobalPkgs    = true;               # Use pkgs set within nixpkgs.
        home-manager.useUserPackages  = true;               # Install packages defined in home-manager.
        home-manager.sharedModules    = homeManagerModules; # Custom modules.
      };
    in inputs.darwin.lib.darwinSystem {
      inherit system;
      modules = darwinModules ++ [commonConfig hostConfig];
    };
}