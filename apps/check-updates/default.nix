{ lib, pkgs, writeNushellScript }:
let
  metadataJson = pkgs.writeText "pinned-metadata.json" (builtins.toJSON pkgs.trackedGithubVersions);
  script = writeNushellScript "check-updates" ./script.nu;
in
pkgs.writeShellApplication {
  name = "check-updates";
  runtimeInputs = [ pkgs.nushell ];
  text = ''
    export METADATA_FILE="${metadataJson}"
    exec nu ${script} "$@"
  '';
  meta.description = "Check for updates on pinned packages";
}
