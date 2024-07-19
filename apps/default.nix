{ nixpkgs }:
let
  lib = nixpkgs.lib;
  merge = lib.foldr (a: b: a // b) { };
  mkApp = pkgFile: system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      appPackage = pkgs.callPackage pkgFile { inherit pkgs; lib = pkgs.lib; };
      name = "${(lib.removeSuffix ".nix" (builtins.baseNameOf pkgFile))}";
    in {
      "${name}" = {
        type = "app";
        program = "${appPackage}/bin/${appPackage.name}";
      };
    };

  mkLinuxApps = lib.genAttrs [ "x86_64-linux" "aarch64-linux" ] (system:
    merge [
     (mkApp ./nixos-install.nix system)
     (mkApp ./dotfiles-install.nix system)
    ]
  );

  mkDarwinApps = lib.genAttrs [ "aarch64-darwin" ] (system:
    merge [
     (mkApp ./darwin-install.nix system)
     (mkApp ./dotfiles-install.nix system)
    ]
  );
in mkLinuxApps // mkDarwinApps
