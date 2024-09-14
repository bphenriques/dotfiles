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

  sharedNixpkgsConfig = pkgs: {
    allowUnfreePredicate = pkg: builtins.elem (pkgs.lib.getName pkg) [
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

      # Fingerprint
      "libfprint-2-tod1-goodix-550a"

      # Cuda
      "libnvjitlink"
      "libnpp"
    ] || (pkgs.lib.strings.hasPrefix "cuda" (pkgs.lib.getName pkg)) || (pkgs.lib.strings.hasPrefix "libcu" (pkgs.lib.getName pkg)); # Cuda

    permittedInsecurePackages = [
      "electron-24.8.6"
      "electron-27.3.11"
      "electron-28.3.3"
    ];
  };

  mkExtraArgs = system: {
    self = {
      pkgs = {
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
      communityPkgs = {
        ghostty = inputs.ghostty.packages.${system}.default;
      };
    };
  };

  mkHomeManagerCommonModule = system: {
    useGlobalPkgs   = true;               # Use pkgs set within nixpkgs.
    useUserPackages = true;               # Install packages defined in home-manager.
    sharedModules   = [
      inputs.sops-nix.homeManagerModules.sops
    ] ++ (lib.attrsets.attrValues inputs.self.homeManagerModules);

    extraSpecialArgs = mkExtraArgs system;
  };
in
{
  mkNixOSHost = { system ? "x86_64-linux", hostConfig }:
    let
      nixpkgs = inputs.nixpkgs-unstable;
      lib = nixpkgs.lib;
      overlays = (lib.attrsets.attrValues inputs.self.overlays) ++ [ inputs.nur.overlay ];

      commonConfig = {
        nixpkgs = {
          inherit overlays;
          config = sharedNixpkgsConfig nixpkgs;
        };
        nix = sharedNixConfig;
        home-manager = mkHomeManagerCommonModule system;
      };
      commonModules = [
        inputs.sops-nix.nixosModules.sops
        inputs.disko.nixosModules.disko
        inputs.home-manager.nixosModules.home-manager
      ] ++ lib.attrsets.attrValues inputs.self.nixosModules;
    in nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = mkExtraArgs system;
      modules = commonModules ++ [ commonConfig hostConfig ];
    };

  mkMacOSHost = { system ? "aarch64-darwin", hostConfig }:
    let
      nixpkgs = inputs.nixpkgs-unstable;
      lib = nixpkgs.lib;
      overlays = (lib.attrsets.attrValues inputs.self.overlays) ++ [ inputs.nur.overlay ];

      darwinModules = [
        inputs.home-manager.darwinModules.home-manager
      ] ++ (lib.attrsets.attrValues inputs.self.darwinModules);

      commonConfig = {
        nixpkgs = {
          inherit overlays;
          hostPlatform = system;
          config = sharedNixpkgsConfig nixpkgs;
        };
        nix = sharedNixConfig;
        home-manager = mkHomeManagerCommonModule system;
      };
    in inputs.darwin.lib.darwinSystem {
      inherit system;
      specialArgs = mkExtraArgs system;
      modules = darwinModules ++ [ commonConfig hostConfig ];
    };
}