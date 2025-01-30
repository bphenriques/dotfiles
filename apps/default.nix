{ nixpkgs, mylib }:
let
  lib = nixpkgs.lib;
  inherit (mylib.builders) forAllSystems forLinuxSystems forDarwinSystems writeLocalCompatibleScriptBin;

  mkApp = mkPackage: pkgs:
    let pkg = mkPackage pkgs; in {
      "${pkg.name}" = {
        type = "app";
        program = "${pkg}/bin/${pkg.name}";
      };
    };

  mkNixOSInstaller = pkgs: writeLocalCompatibleScriptBin pkgs {
    name = "nixos-install";
    runtimeInputs = [ (mkBitwardenSession pkgs) pkgs.yq-go pkgs.jq ];
    text = pkgs.lib.fileContents ./nixos-install.sh;
  };

  mkDarwinInstall = pkgs: writeLocalCompatibleScriptBin pkgs {
    name = "darwin-install";
    runtimeInputs = [ ];
    text = pkgs.lib.fileContents ./darwin-install.sh;
  };

  mkPostInstall = pkgs: writeLocalCompatibleScriptBin pkgs {
    name = "post-install";
    runtimeInputs = [ pkgs.git pkgs.yq-go pkgs.age pkgs.sops pkgs.gnupg (mkBitwardenSession pkgs) ];
    text = pkgs.lib.fileContents ./post-install.sh;
  };

  mkBitwardenSession = pkgs: writeLocalCompatibleScriptBin pkgs {
    name = "bw-session";
    runtimeInputs = [ pkgs.bitwarden-cli ];
    text = pkgs.lib.fileContents ./bw-session.sh;
  };

  crossPlatformApps = forAllSystems (system:
    lib.attrsets.mergeAttrsList [
     (mkApp mkPostInstall nixpkgs.legacyPackages.${system})
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
