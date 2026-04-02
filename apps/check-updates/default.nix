{ lib, pkgs, writeNushellScript }:
let
  packagesJson = pkgs.writeText "packages-metadata.json" (builtins.toJSON pkgs.trackedGithubVersions);
  containersJson = pkgs.writeText "containers-metadata.json" (builtins.toJSON pkgs.trackedContainerVersions);
  script = writeNushellScript "check-updates" ./script.nu;
in
pkgs.writeShellApplication {
  name = "check-updates";
  runtimeInputs = [ pkgs.nushell ];
  text = ''
    export PACKAGES_FILE="${packagesJson}"
    export CONTAINERS_FILE="${containersJson}"
    exec nu ${script} "$@"
  '';
  meta.description = "Check for updates on pinned packages and container images";
}
