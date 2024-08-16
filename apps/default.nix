{ nixpkgs }:
let
  lib = nixpkgs.lib;
  merge = lib.foldr (a: b: a // b) { };

  # Replace the interpreter's location to be one under the nix store.
  patchShebangs = pkg: pkg.overrideAttrs(old: {
    buildCommand = "${old.buildCommand}\n patchShebangs $out";
  });

  mkDarwinInstall = pkgs: patchShebangs (pkgs.writeShellApplication {
    name = "darwin-install";
    runtimeInputs = with pkgs; [ ];
    text = lib.fileContents ./darwin-install.sh;
  });

  mkDotfilesInstall = pkgs: patchShebangs (pkgs.writeShellApplication {
    name = "dotfiles-install";
    runtimeInputs = with pkgs; [ git yq-go age sops gnupg bitwarden-cli ];
    text = lib.fileContents ./dotfiles-install.sh;
  });

  mkApp = mkPackage: system:
    let pkg = mkPackage nixpkgs.legacyPackages.${system};
    in
    {
      "${pkg.name}" = {
        type = "app";
        program = "${pkg}/bin/${pkg.name}";
      };
    };

  mkLinuxApps = lib.genAttrs [ "x86_64-linux" "aarch64-linux" ] (system:
    merge [
     (mkApp mkDotfilesInstall system)
    ]
  );

  mkDarwinApps = lib.genAttrs [ "aarch64-darwin" ] (system:
    merge [
     (mkApp mkDarwinInstall system)
     (mkApp mkDotfilesInstall system)
    ]
  );
in mkLinuxApps // mkDarwinApps
