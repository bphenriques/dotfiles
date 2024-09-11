{ nixpkgs, nixpkgs-unstable }:
let
  lib = nixpkgs.lib;

  forAllSystems = lib.genAttrs [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
  forDarwinSystems = lib.genAttrs [ "aarch64-darwin" ];
  forLinuxSystems = lib.genAttrs [ "x86_64-linux" "aarch64-linux" ];

  # Replace the interpreter's location to be one under the nix store.
  # - writeShellApplication does not support that.
  # - we now have to pass the dependencies by-hand
  # - we now lose shellchecks at build time. It is fine in this case as I usually run by hand.
  #
  # Why: I like the option of running the scripts locally.
  writeLocalCompatibleScriptBin = pkgs: { name, text, runtimeInputs }:
    let
      patchShebangs = pkg: pkg.overrideAttrs(old: {
        buildCommand = "${old.buildCommand}\n patchShebangs $out";
      });
    in pkgs.symlinkJoin {
      inherit name;
      paths = [ (patchShebangs (pkgs.writeScriptBin name text)) ] ++ runtimeInputs;
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = "wrapProgram $out/bin/${name} --prefix PATH : $out/bin";
    };

  mkNixOSInstaller = pkgs:
    writeLocalCompatibleScriptBin pkgs {
      name = "nixos-install";
      runtimeInputs = with pkgs; [ (mkBitwardenSession pkgs) yq-go jq ];
      text = pkgs.lib.fileContents ./nixos-install.sh;
    };

  mkDarwinInstall = pkgs:
    writeLocalCompatibleScriptBin pkgs {
      name = "darwin-install";
      runtimeInputs = with pkgs; [ ];
      text = pkgs.lib.fileContents ./darwin-install.sh;
    };

  mkDotfilesInstall = pkgs:
    writeLocalCompatibleScriptBin pkgs {
      name = "dotfiles-install";
      runtimeInputs = with pkgs; [ git yq-go age sops gnupg (mkBitwardenSession pkgs) ];
      text = pkgs.lib.fileContents ./dotfiles-install.sh;
    };

  mkBitwardenSession = pkgs:
    writeLocalCompatibleScriptBin pkgs {
      name = "bw-session";
      runtimeInputs = with pkgs; [ bitwarden-cli ];
      text = pkgs.lib.fileContents ./bw-session.sh;
    };

  mkSopsGitFilter = pkgs:
    writeLocalCompatibleScriptBin pkgs {
      name = "sops-git-filter";
      runtimeInputs = with pkgs; [ git sops ];
      text = pkgs.lib.fileContents ./sops-git-filter.sh;
    };

  mkApp = mkPackage: pkgs:
    let pkg = mkPackage pkgs; in {
      "${pkg.name}" = {
        type = "app";
        program = "${pkg}/bin/${pkg.name}";
      };
    };

  crossPlatformApps = forAllSystems (system:
    lib.attrsets.mergeAttrsList [
     (mkApp mkDotfilesInstall nixpkgs.legacyPackages.${system})
     (mkApp mkSopsGitFilter nixpkgs-unstable.legacyPackages.${system})  # TODO: move to stable once it reaches 3.9.0
    ]
  );

  linuxApps = forLinuxSystems (system:
    lib.attrsets.mergeAttrsList [
     (mkApp mkNixOSInstaller nixpkgs.legacyPackages.${system})
    ]
  );

  darwinApps = forDarwinSystems (system:
    lib.attrsets.mergeAttrsList [
     (mkApp mkDarwinInstall nixpkgs.legacyPackages.${system})
    ]
  );
in forAllSystems (system:
  lib.attrsets.mergeAttrsList [
    crossPlatformApps.${system}
    (linuxApps.${system} or { })
    (darwinApps.${system} or { })
  ]
)
