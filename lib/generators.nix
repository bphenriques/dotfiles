{ lib }:
let
  inherit (builtins) baseNameOf dirOf replaceStrings listToAttrs;
  inherit (lib) removeSuffix map;
  inherit (lib.filesystem) listFilesRecursive;
in
{
  # Generates a attr with { "{dir}-{file}" = import {file-path}; }
  getModulesAttrs = dir: let
      targetFiles = (lib.filter (path: (baseNameOf path) != "default.nix") (listFilesRecursive dir));
      toModuleName = path: (baseNameOf (dirOf path)) + "-" + (removeSuffix ".nix" (baseNameOf path));
    in listToAttrs (map (path: { name = toModuleName path; value = import path; }) targetFiles);
}