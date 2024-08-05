{ inputs, ... }:
# TODO: https://gitlab.com/usmcamp0811/dotfiles/-/tree/nixos/modules/nixos?ref_type=heads

let
  inherit (builtins) readDir attrNames;
  inherit (inputs.nixpkgs.lib) filterAttrs genAttrs foldl';

  # Helpers
  getDirs = from: attrNames (filterAttrs (_ : type: type == "directory") (readDir from));

  # The different types of overlays
  add-custom-packages = final: prev: genAttrs (getDirs ./.) (pkgName: final.callPackage (./. + "/${pkgName}") {} );
  add-external-flakes = [
    (final: prev: { zjstatus = inputs.zjstatus.packages.${prev.system}.default; })
    (final: prev: { ghostty = inputs.ghostty.packages.${prev.system}.default; })
    inputs.nur.overlay
  ];

  # Add unstable inside packages for bleeding-edge packages. Home-Manager follows unstable, therefore there is little sense for this.
  # add-unstable-packages = final: prev: {
  #   unstable = import inputs.nixpkgs-unstable {
  #     system = final.system;
  #   };
  #   unstable = import inputs.nixpkgs-unstable {
  #     system = final.system;
  #   };
  # };

  # Automatically group fish plugins inside a directory
  add-fish-plugins = final: prev: {
    fishPlugins = prev.fishPlugins.overrideScope (finalx: prevx:
      (foldl' (acc: packageName: acc // {
         "${packageName}" = prevx.buildFishPlugin {
           pname = packageName;
           version = "latest";
           src = ./fish-plugins + "/${packageName}";
         };
       }) { } (getDirs ./fish-plugins)));
  };

in [add-custom-packages inputs.nur.overlay add-fish-plugins] ++ add-external-flakes
