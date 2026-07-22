{ pkgs, lib, cvVm, fleetFacts, ... }:
let
  inherit (fleetFacts) services;

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

  # Each name links to its upstream homepage (meta.homepage) so a visitor can see what the service is.
  renderItem =
    s:
    let
      label =
        if s.homepage != null then
          ''<a href="${s.homepage}" target="_blank" rel="noopener">${s.displayName}</a>''
        else
          s.displayName;
    in
    "<li>${label}</li>";
  renderCategory = c: ''<li class="cat">${c}</li>'' + lib.concatMapStrings renderItem (inCategory c);
  servicesHtml = ''<ul class="svc">'' + lib.concatMapStrings renderCategory categories + "</ul>";

  site = pkgs.runCommandLocal "cv-site" { } ''
    cp -r ${./site} "$out"
    chmod -R u+w "$out"
    substituteInPlace "$out/index.html" \
      --replace-warn '@serviceCount@' '${toString (builtins.length services)}' \
      --replace-warn '@services@' ${lib.escapeShellArg servicesHtml}
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
