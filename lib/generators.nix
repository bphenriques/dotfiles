{ lib }:
let
  inherit (builtins) baseNameOf listToAttrs;
  inherit (lib) removeSuffix map nameValuePair filter genAttrs mergeAttrsList;
  inherit (lib.filesystem) listFilesRecursive;
in
rec {
  forAllSystems = genAttrs [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
  forDarwinSystems = genAttrs [ "aarch64-darwin" ];
  forLinuxSystems = genAttrs [ "x86_64-linux" "aarch64-linux" ];

  mergeAllSystems = attrs: forAllSystems (system: mergeAttrsList (map (attrs: attrs.${system} or { }) attrs));

  # Generates a attr with { "{dir}-{filename}" = {file-path}; }
  readModulesAttrs = dir:
    let
      isNixModule = path:
        let name = baseNameOf path;
        in lib.hasSuffix ".nix" name
           && name != "default.nix"
           && !lib.hasPrefix "_" name;  # Skip files starting with underscore
      targetFiles = filter isNixModule (listFilesRecursive dir);
      toModuleName = path: (baseNameOf (dirOf path)) + "-" + (removeSuffix ".nix" (baseNameOf path));
    in listToAttrs (map (path: nameValuePair (toModuleName path) path) targetFiles);
}
