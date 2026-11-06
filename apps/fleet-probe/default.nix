{ lib, pkgs, self }:
let
  configOf = name: self.nixosConfigurations.${name}.config;
  inherit ((configOf "compute").fleet.lan) hosts;

  # Each probe asserts that `from` reaching `to`:`port` is `expect` (open or blocked). `control` must reach it
  # first, so "blocked" can only mean `from` was filtered. Sources are ssh destinations; guests jump via ssh config.
  toLine = p: "${p.from} ${p.control} ${p.to} ${toString p.port} ${p.expect} ${p.label}";

  # Each microVM guest's egress seal, from the tables its nft rules are built from.
  guestProbes = vmHost:
    let
      config = configOf vmHost;
      hostNameOf = lib.mapAttrs' (name: ip: lib.nameValuePair ip name) hosts;
      inherit (config.my.microvm.host) bridge guests;
    in
    lib.concatLists (lib.mapAttrsToList (name: g:
      let guest = p: { from = name; control = vmHost; } // p; in
      map guest (
        [
          { to = config.fleet.lan.gateway; port = 80; expect = "blocked"; label = "LAN"; }
          { to = bridge.gateway; port = 22; expect = "blocked"; label = "host"; }
          { to = "1.1.1.1"; port = 443; expect = "open"; label = "internet"; }
        ]
        ++ lib.concatMap (port: [
          { to = bridge.gateway; inherit port; expect = "open"; label = "host port"; }
          { to = hosts.${vmHost}; inherit port; expect = "blocked"; label = "host port on LAN"; }
        ]) g.egress.allowHostPorts
        ++ lib.concatMap (e:
          let to = hosts.${e.host} or e.host; label = hostNameOf.${to} or e.host; in
          map (port: { inherit to port label; expect = "open"; }) e.ports
          ++ [ { inherit to; port = 22; expect = "blocked"; label = "${label}, other ports"; } ]
        ) g.egress.allowLan
      )
    ) guests);

  # Host firewalls seen from a LAN peer they do not admit, with compute as the control.
  hostProbes =
    let
      ai = (configOf "ai").fleet.ai;
      storageExporters = (configOf "storage").services.prometheus.exporters;
      compute = configOf "compute";
    in
    [
      { from = "storage"; control = "compute"; to = hosts.compute; port = 443; expect = "open"; label = "compute: Traefik"; }
      { from = "storage"; control = "compute"; to = hosts.compute; port = compute.selfhost.apps.gitea.ssh.port; expect = "open"; label = "compute: Gitea SSH"; }
      { from = "storage"; control = "compute"; to = hosts.compute; port = compute.selfhost.services.jellyfin.port; expect = "blocked"; label = "compute: Jellyfin only via Traefik"; }
      { from = "storage"; control = "compute"; to = hosts.ai; port = ai.endpoint.port; expect = "blocked"; label = "ai: inference endpoint"; }
      { from = "storage"; control = "compute"; to = hosts.ai; port = ai.imageEndpoint.port; expect = "blocked"; label = "ai: ComfyUI"; }
      { from = "ai"; control = "compute"; to = hosts.storage; port = storageExporters.node.port; expect = "blocked"; label = "storage: node exporter"; }
    ];
in
pkgs.writeShellApplication {
  name = "fleet-probe";
  runtimeInputs = [ pkgs.openssh ];
  runtimeEnv.PROBES = lib.concatMapStringsSep "\n" toLine (
    hostProbes ++ lib.concatMap guestProbes (lib.attrNames (import ../../hosts/microvms.nix))
  );
  text = lib.fileContents ./script.sh;
  meta.description = "Check from elsewhere on the LAN that each host's firewall and guest's seal hold on the live kernel";
  meta.platforms = lib.platforms.linux;
}
