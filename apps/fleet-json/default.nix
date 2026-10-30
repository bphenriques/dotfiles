{ lib, pkgs, fleetFacts, ... }:
let
  # Public subset only: publicUrls are LAN addresses.
  public = {
    inherit (fleetFacts) hosts;
    services = lib.listToAttrs (map (s: lib.nameValuePair s.name { inherit (s) displayName homepage category order listed; }) fleetFacts.services);
  };
  json = pkgs.runCommandLocal "fleet.json" { nativeBuildInputs = [ pkgs.jq ]; } ''
    jq . ${pkgs.writeText "fleet-raw.json" (builtins.toJSON public)} > "$out"
  '';
in
pkgs.writeShellApplication {
  name = "fleet-json";
  runtimeInputs = [ pkgs.git ];
  text = ''cp --no-preserve=mode ${json} "$(git rev-parse --show-toplevel)/fleet.json"'';
  passthru = { inherit json; };
  meta.description = "Regenerate fleet.json, the public fleet inventory consumed by bphenriques.com";
  meta.platforms = lib.platforms.all;
}
