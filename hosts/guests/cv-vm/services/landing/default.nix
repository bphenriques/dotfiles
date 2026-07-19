# darkhttpd serving the store-baked static site (bentopdf recipe); hardened DynamicUser, localhost-only.
{ pkgs, lib, cvVm, fleetFacts, ... }:
let
  # Templated at build time (design lands later); fleet facts substituted in now.
  site = pkgs.runCommandLocal "cv-site" { } ''
    cp -r ${./site} "$out"
    chmod -R u+w "$out"
    substituteInPlace "$out/index.html" \
      --replace-warn '@hostCount@' '${toString fleetFacts.hosts}' \
      --replace-warn '@serviceCount@' '${toString fleetFacts.services}'
  '';
in
{
  systemd.services.landing = {
    description = "Static CV landing page";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
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
