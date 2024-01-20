{ inputs, ... }:
let
  inherit (builtins) readDir attrNames;
  inherit (inputs.nixpkgs.lib) filterAttrs genAttrs foldl';

  # Helpers
  getDirs = from: attrNames (filterAttrs (_ : type: type == "directory") (readDir from));

  # The different types of overlays
  newPackages = final: prev: genAttrs (getDirs ./.) (pkgName: final.callPackage (./. + "/${pkgName}") {} );
  externalFlakes = with inputs; [
    (final: prev: { zjstatus = zjstatus.packages.${prev.system}.default; })
  ];

  fishPluginsOverride = final: prev: {
    fishPlugins = prev.fishPlugins.overrideScope' (finalx: prevx:
      (foldl' (acc: packageName: acc // {
         "${packageName}" = prevx.buildFishPlugin {
           pname = packageName;
           version = "latest";
           src = ./fish-plugins + "/${packageName}";
         };
       }) { } (getDirs ./fish-plugins)));
  };

in [newPackages inputs.nur.overlay fishPluginsOverride] ++ externalFlakes

# Example on how to bring unstable within scope https://github.com/ethanabrooks/nix/blob/main/overlays/default.nix#L18
