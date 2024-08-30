{ nixpkgs }:
let
  lib = nixpkgs.lib;

  # Replace the interpreter's location to be one under the nix store.
  patchShebangs = pkg: pkg.overrideAttrs(old: {
    buildCommand = "${old.buildCommand}\n patchShebangs $out";
  });

  mkNixOSInstaller = pkgs: patchShebangs (pkgs.writeShellApplication {
    name = "nixos-install";
    runtimeInputs = with pkgs; [ (mkBitwardenSession pkgs) yq-go jq ];
    text = lib.fileContents ./nixos-install.sh;
  });

  mkDarwinInstall = pkgs: patchShebangs (pkgs.writeShellApplication {
    name = "darwin-install";
    runtimeInputs = with pkgs; [ ];
    text = lib.fileContents ./darwin-install.sh;
  });

  mkDotfilesInstall = pkgs: patchShebangs (pkgs.writeShellApplication {
    name = "dotfiles-install";
    runtimeInputs = with pkgs; [ git yq-go age sops gnupg (mkBitwardenSession pkgs) ];
    text = lib.fileContents ./dotfiles-install.sh;
  });

  mkBitwardenSession = pkgs: patchShebangs (pkgs.writeShellApplication {
    name = "bw-session";
    runtimeInputs = with pkgs; [ bitwarden-cli ];
    text = lib.fileContents ./bw-session.sh;
  });

  mkSopsGitFilter = pkgs: patchShebangs (pkgs.writeShellApplication {
    name = "sops-git-filter";
    runtimeInputs = with pkgs; [ git sops ];
    text = lib.fileContents ./sops-git-filter.sh;
  });

  mkApp = mkPackage: system:
    let pkg = mkPackage nixpkgs.legacyPackages.${system}; in {
      "${pkg.name}" = {
        type = "app";
        program = "${pkg}/bin/${pkg.name}";
      };
    };

  mkLinuxApps = lib.genAttrs [ "x86_64-linux" "aarch64-linux" ] (system:
    lib.attrsets.mergeAttrsList [
     (mkApp mkDotfilesInstall system)
     (mkApp mkBitwardenSession system)
     (mkApp mkNixOSInstaller system)
     (mkApp mkSopsGitFilter system)
    ]
  );

  mkDarwinApps = lib.genAttrs [ "aarch64-darwin" ] (system:
    lib.attrsets.mergeAttrsList [
     (mkApp mkDarwinInstall system)
     (mkApp mkDotfilesInstall system)
     (mkApp mkBitwardenSession system)
     (mkApp mkSopsGitFilter system)
    ]
  );
in mkLinuxApps // mkDarwinApps
