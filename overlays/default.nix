{ inputs, ... }:
let
  inherit (builtins) readDir attrNames;
  inherit (inputs.nixpkgs.lib) filterAttrs genAttrs;

  getDirs = from: attrNames (filterAttrs (_ : type: type == "directory") (readDir from));
  additions = final: prev: genAttrs (getDirs ./.) (pkgName: final.callPackage (./. + "/${pkgName}") {} );

in [additions]

# Example on how to bring unstable within scope https://github.com/ethanabrooks/nix/blob/main/overlays/default.nix#L18
