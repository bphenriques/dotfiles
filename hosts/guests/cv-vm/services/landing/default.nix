{ pkgs, lib, cvVm, fleetFacts, ... }:
let
  inherit (fleetFacts) hosts services;

  # Group order; any category not listed falls to the end, alphabetically.
  categoryOrder = [ "identity" "media" "downloads" "productivity" "files" "home" "monitoring" ];
  usedCategories = lib.unique (map (s: s.category) services);
  categories =
    (lib.filter (c: lib.elem c usedCategories) categoryOrder)
    ++ lib.sort (a: b: a < b) (lib.filter (c: !lib.elem c categoryOrder) usedCategories);
  inCategory =
    c:
    lib.sort (a: b: if a.order != b.order then a.order < b.order else a.displayName < b.displayName) (
      lib.filter (s: s.category == c) services
    );

  # Page data, kept separate from the markup (index.html.mustache) and rendered at build time.
  data = {
    hostCount = hosts;
    serviceCount = builtins.length services;
    categories = map (c: {
      name = c;
      services = map (s: { inherit (s) displayName homepage; }) (inCategory c);
    }) categories;
  };
  dataJson = pkgs.writeText "cv-data.json" (builtins.toJSON data);

  site = pkgs.runCommandLocal "cv-site" { nativeBuildInputs = [ pkgs.mustache-go ]; } ''
    cp -r ${./site} "$out"
    chmod -R u+w "$out"
    mustache ${dataJson} "$out/index.html.mustache" > "$out/index.html"
    rm "$out/index.html.mustache"
  '';
in
{
  systemd.services.landing = {
    description = "Static CV landing page";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = "${lib.getExe' pkgs.darkhttpd "darkhttpd"} ${site} --addr 127.0.0.1 --port ${toString cvVm.staticPort} --no-listing --no-server-id";
      DynamicUser = true;
      Restart = "on-failure";
      RestartSec = "10s";
      # Full confinement for the one internet-facing process (via Funnel): it only binds a
      # localhost socket and reads static files, so it needs no caps, no extra syscalls, no LAN.
      CapabilityBoundingSet = "";
      NoNewPrivileges = true;
      SystemCallFilter = [ "@system-service" ];
      SystemCallArchitectures = "native";
      RestrictAddressFamilies = [ "AF_INET" ];
      RestrictNamespaces = true;
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
      LockPersonality = true;
      MemoryDenyWriteExecute = true;
      PrivateDevices = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      ProtectClock = true;
      ProtectHostname = true;
      ProtectKernelTunables = true;
      ProtectKernelModules = true;
      ProtectKernelLogs = true;
      ProtectControlGroups = true;
      ProtectProc = "invisible";
      ProcSubset = "pid";
      RemoveIPC = true;
      UMask = "0077";
      IPAddressDeny = "any";
      IPAddressAllow = "localhost";
    };
  };
}
