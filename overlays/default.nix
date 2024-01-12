{ inputs, ... }:
let
  inherit (builtins) readDir attrNames;
  inherit (inputs.nixpkgs.lib) filterAttrs genAttrs;

  getDirs = from: attrNames (filterAttrs (_ : type: type == "directory") (readDir from));
  newPackages = final: prev: genAttrs (getDirs ./.) (pkgName: final.callPackage (./. + "/${pkgName}") {} );

  externalFlakes = with inputs; [
    (final: prev: { zjstatus = zjstatus.packages.${prev.system}.default; })
  ];
in [newPackages inputs.nur.overlay] ++ externalFlakes

# Example on how to bring unstable within scope https://github.com/ethanabrooks/nix/blob/main/overlays/default.nix#L18
