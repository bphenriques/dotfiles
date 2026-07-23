{ config, pkgs, lib, ... }:
{
  # EnvironmentFile is read by systemd as root, so DynamicUser needn't access the secret.
  sops.secrets."cloudflared/token" = { };
  sops.templates."cloudflared-env".content = "TUNNEL_TOKEN=${config.sops.placeholder."cloudflared/token"}";

  systemd.services.cloudflared = {
    description = "Cloudflare Tunnel (cv-vm apex landing)";
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    serviceConfig = {
      EnvironmentFile = config.sops.templates."cloudflared-env".path;
      ExecStart = "${lib.getExe pkgs.cloudflared} tunnel --no-autoupdate run";
      Restart = "on-failure";
      RestartSec = "10s";

      # No IPAddress lock: needs the CF edge; the VM egress policy already blocks the LAN.
      DynamicUser = true;
      CapabilityBoundingSet = "";
      NoNewPrivileges = true;
      RestrictAddressFamilies = [ "AF_INET" "AF_INET6" ];
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
      SystemCallFilter = [ "@system-service" ];
      SystemCallArchitectures = "native";
    };
  };
}
