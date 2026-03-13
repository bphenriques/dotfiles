{ lib }:
let
  inherit (builtins) baseNameOf listToAttrs toString;
  inherit (lib) removeSuffix map nameValuePair filter genAttrs mergeAttrsList splitString concatStringsSep removePrefix any hasInfix;
  inherit (lib.filesystem) listFilesRecursive;
in
rec {
  forAllSystems = genAttrs [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
  forDarwinSystems = genAttrs [ "aarch64-darwin" ];
  forLinuxSystems = genAttrs [ "x86_64-linux" "aarch64-linux" ];

  mergeAllSystems = attrs: forAllSystems (system: mergeAttrsList (map (attrs: attrs.${system} or { }) attrs));

  # Generates an attrset { "path-like-name" = <module-path>; } from .nix files.
  # Skips default modules and internal implementation folders.
  readModulesAttrs = dir:
    let
      rootPath = toString dir;
      ignoredDirs = [ "/schemas/" "/internal/" "/lib/" ];
      relPath = path: removePrefix "${rootPath}/" (toString path);
      isExportedModule = path: let
        rel = relPath path;
        fileName = baseNameOf rel;
      in lib.hasSuffix ".nix" fileName && fileName != "default.nix" && !(any (dirName: hasInfix dirName "/${rel}") ignoredDirs);
      toModuleName = path: removeSuffix ".nix" (concatStringsSep "-" (splitString "/" (relPath path)));
    in
      listToAttrs (map (path: nameValuePair (toModuleName path) path) (filter isExportedModule (listFilesRecursive dir)));
}
