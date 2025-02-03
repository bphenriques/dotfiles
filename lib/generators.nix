{ lib }:
let
  inherit (builtins) baseNameOf listToAttrs;
  inherit (lib) removeSuffix map;
  inherit (lib.filesystem) listFilesRecursive;
in
{
  # Generates a attr with { "{filename}" = import {file-path}; }
  readModulesAttrs = dir:
    let
      targetFiles = lib.filter (path: (baseNameOf path) != "default.nix") (listFilesRecursive dir);
      toModuleName = path: removeSuffix ".nix" (baseNameOf path);
    in
      listToAttrs (map (path: { name = toModuleName path; value = import path; }) targetFiles);
}