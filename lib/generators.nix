{ lib }:
let
  inherit (builtins) baseNameOf listToAttrs;
  inherit (lib) removeSuffix map;
  inherit (lib.filesystem) listFilesRecursive;
in
rec {
  forAllSystems = lib.genAttrs [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
  forDarwinSystems = lib.genAttrs [ "aarch64-darwin" ];
  forLinuxSystems = lib.genAttrs [ "x86_64-linux" "aarch64-linux" ];

  mergeAllSystems = attrs: forAllSystems (system: lib.mergeAttrsList (lib.map (attrs: attrs.${system} or { }) attrs));

  # Generates a attr with { "{dir}-{filename}" = {file-path}; }
  readModulesAttrs = dir:
    let
      targetFiles = lib.filter (path: (baseNameOf path) != "default.nix") (listFilesRecursive dir);
      toModuleName = path: (baseNameOf (dirOf path)) + "-" + (removeSuffix ".nix" (baseNameOf path));
    in
      listToAttrs (map (path: { name = toModuleName path; value = path; }) targetFiles);
}
