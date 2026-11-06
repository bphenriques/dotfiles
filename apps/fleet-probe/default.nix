{ lib, pkgs, self }:
let
  configOf = name: self.nixosConfigurations.${name}.config;
  inherit ((configOf "compute").fleet.lan) hosts;

  # "<from> <control> <host> <port> <open|blocked> <label>": <control> must reach the target first, so
  # "blocked" can only mean <from> was filtered. Sources are ssh destinations; guests jump via ssh config.
  probe = from: control: host: port: want: label: "${from} ${control} ${host} ${toString port} ${want} ${label}";

  # Each microVM guest's egress seal, from the tables its nft rules are built from.
  guestProbes = vmHost:
    let
      config = configOf vmHost;
      hostNameOf = lib.mapAttrs' (name: ip: lib.nameValuePair ip name) hosts;
      inherit (config.my.microvm.host) bridge guests;
    in
    lib.concatLists (lib.mapAttrsToList (name: g:
      let guest = probe name vmHost; in
      [
        (guest config.fleet.lan.gateway 80 "blocked" "LAN")
        (guest bridge.gateway 22 "blocked" "host")
        (guest "1.1.1.1" 443 "open" "internet")
      ]
      ++ lib.concatMap (p: [
        (guest bridge.gateway p "open" "host port")
        (guest hosts.${vmHost} p "blocked" "host port on LAN")
      ]) g.egress.allowHostPorts
      ++ lib.concatMap (e:
        let target = hosts.${e.host} or e.host; label = hostNameOf.${target} or e.host; in
        map (p: guest target p "open" label) e.ports
        ++ [ (guest target 22 "blocked" "${label}, other ports") ]
      ) g.egress.allowLan
    ) guests);

  # Host firewalls seen from a LAN peer they do not admit, with compute as the control.
  hostProbes =
    let
      ai = (configOf "ai").fleet.ai;
      storageExporters = (configOf "storage").services.prometheus.exporters;
      compute = configOf "compute";
    in
    [
      (probe "storage" "compute" hosts.compute 443 "open" "compute: Traefik")
      (probe "storage" "compute" hosts.compute compute.selfhost.apps.gitea.ssh.port "open" "compute: Gitea SSH")
      (probe "storage" "compute" hosts.compute compute.selfhost.services.jellyfin.port "blocked" "compute: Jellyfin only via Traefik")
      (probe "storage" "compute" hosts.ai ai.endpoint.port "blocked" "ai: inference endpoint")
      (probe "storage" "compute" hosts.ai ai.imageEndpoint.port "blocked" "ai: ComfyUI")
      (probe "ai" "compute" hosts.storage storageExporters.node.port "blocked" "storage: node exporter")
    ];
in
pkgs.writeShellApplication {
  name = "fleet-probe";
  runtimeInputs = [ pkgs.openssh ];
  runtimeEnv.PROBES = lib.concatStringsSep "\n" (
    hostProbes ++ lib.concatMap guestProbes (lib.attrNames (import ../../hosts/microvms.nix))
  );
  text = lib.fileContents ./script.sh;
  meta.description = "Check from elsewhere on the LAN that each host's firewall and guest's seal hold on the live kernel";
  meta.platforms = lib.platforms.linux;
}
