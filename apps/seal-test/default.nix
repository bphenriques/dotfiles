{ lib, pkgs, self, ... }:
let
  # One "<jump> <guest> <host> <port> <open|blocked> <label>" line per probe, from the tables the nft rules are built from.
  probesFor = vmHost:
    let
      config = self.nixosConfigurations.${vmHost}.config;
      inherit (config.fleet.lan) hosts;
      inherit (config.my.microvm.host) bridge guests;
    in
    lib.concatLists (lib.mapAttrsToList (name: g:
      map (p: "${vmHost} ${name} ${p}") (
        [
          "${hosts.storage} 445 blocked LAN"
          "${bridge.gateway} 22 blocked host"
          "1.1.1.1 443 open internet"
        ]
        ++ lib.concatMap (p: [
          "${bridge.gateway} ${toString p} open host port"
          "${hosts.${vmHost}} ${toString p} blocked host port on LAN"
        ]) g.egress.allowHostPorts
        ++ lib.concatMap (e:
          let target = hosts.${e.host} or e.host; in
          map (p: "${target} ${toString p} open ${e.host}") e.ports
          ++ [ "${target} 22 blocked ${e.host}, other ports" ]
        ) g.egress.allowLan
      )
    ) guests);
in
pkgs.writeShellApplication {
  name = "seal-test";
  runtimeInputs = [ pkgs.openssh ];
  runtimeEnv.PROBES = lib.concatStringsSep "\n" (lib.concatMap probesFor (lib.attrNames (import ../../hosts/microvms.nix)));
  text = lib.fileContents ./script.sh;
  meta.description = "Check that each microVM guest's network seal holds on the live kernel";
  meta.platforms = lib.platforms.linux;
}
