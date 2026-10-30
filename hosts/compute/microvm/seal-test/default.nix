{ lib, pkgs, self, ... }:
let
  compute = self.nixosConfigurations.compute.config;
  inherit (compute.fleet.lan) hosts;
  inherit (compute.my.microvm.host) bridge guests;

  # One "<guest> <host> <port> <open|blocked> <label>" line per probe, from the table the nft rules are built from.
  probes = lib.concatLists (lib.mapAttrsToList (name: g:
    [
      "${name} ${hosts.storage} 445 blocked LAN"
      "${name} ${bridge.gateway} 22 blocked host"
      "${name} 1.1.1.1 443 open internet"
    ]
    ++ lib.concatMap (p: [
      "${name} ${bridge.gateway} ${toString p} open host port"
      "${name} ${hosts.compute} ${toString p} blocked host port on LAN"
    ]) g.egress.allowHostPorts
    ++ lib.concatMap (e:
      let target = hosts.${e.host} or e.host; in
      map (p: "${name} ${target} ${toString p} open ${e.host}") e.ports
      ++ [ "${name} ${target} 22 blocked ${e.host}, other ports" ]
    ) g.egress.allowLan
  ) guests);
in
pkgs.writeShellApplication {
  name = "seal-test";
  runtimeInputs = [ pkgs.openssh ];
  runtimeEnv.PROBES = lib.concatStringsSep "\n" probes;
  text = lib.fileContents ./script.sh;
  meta.description = "Check that each microVM guest's network seal holds on the live kernel";
  meta.platforms = lib.platforms.linux;
}
