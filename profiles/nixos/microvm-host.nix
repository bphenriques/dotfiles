# Host-agnostic microVM host: bridge + NAT + egress seal + per-guest taps/sandboxes/monitoring,
# all driven by the allocation table (homelab.microvm.host.guests). No host name baked in.
{ config, lib, self, inputs, ... }:
let
  cfg = config.homelab.microvm.host;
  inherit (cfg) bridge;

  # Confines the unprivileged VMM to this VM's state; cloud-hypervisor is tap-native so empty caps don't break networking.
  vmSandbox = {
    ProtectSystem = "strict";
    PrivateTmp = true;
    ProtectHome = true;
    NoNewPrivileges = true;
    CapabilityBoundingSet = "";
    LockPersonality = true;
    RestrictSUIDSGID = true;
    ProtectClock = true;
    ProtectKernelTunables = true;
    ProtectKernelModules = true;
    ProtectControlGroups = true;
    ProtectProc = "invisible";
    ProcSubset = "pid";
    DevicePolicy = "closed";                # keep host /dev but restrict to what the VMM opens
    DeviceAllow = [ "/dev/kvm rw" "/dev/net/tun rw" ];
    RestrictNamespaces = true;
    SystemCallArchitectures = "native";
    RestrictRealtime = true;
    MemoryDenyWriteExecute = true;
    RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" "AF_INET6" "AF_NETLINK" ];
    SystemCallFilter = [ "@system-service" ];
  };

  mkScope = name: g: {
    scrapeConfigs = [
      { job_name = name; static_configs = [{ targets = [ "${g.ip}:9100" ]; labels.instance = name; }]; }
    ] ++ lib.optional g.monitoring.traefikMetrics
      { job_name = "${name}-traefik"; static_configs = [{ targets = [ "${g.ip}:9117" ]; labels.instance = name; }]; };
    rules = [{
      inherit name;
      rules = [
        { alert = "GuestDown"; expr = ''up{job="${name}"} == 0''; for = "5m"; labels.severity = "warning"; annotations.summary = "${name} is unreachable"; }
        { alert = "GuestHighCpu"; expr = ''(1 - avg by(instance) (rate(node_cpu_seconds_total{instance="${name}",mode="idle"}[5m]))) * 100 > 90''; for = "15m"; labels.severity = "warning"; annotations.summary = "${name} CPU over 90% for 15m"; }
      ] ++ lib.optional (g.monitoring.storageMount != null)
        { alert = "GuestStorageNearCap"; expr = ''node_filesystem_avail_bytes{mountpoint="${g.monitoring.storageMount}",fstype="ext4"} / node_filesystem_size_bytes{mountpoint="${g.monitoring.storageMount}",fstype="ext4"} < 0.1''; for = "15m"; labels.severity = "warning"; annotations.summary = "${g.monitoring.storageMount} on ${name} is over 90% full"; };
    }];
  };

  # LAN egress allowlist (default none), flattened once for both the nft accepts and the assertions.
  ownIp = config.custom.fleet.lan.hosts.${config.networking.hostName} or null;
  resolveHost = h: config.custom.fleet.lan.hosts.${h} or h;
  egressEntries = lib.concatLists (lib.mapAttrsToList (name: g:
    map (e: { inherit name; inherit (g) ip; inherit (e) host ports; target = resolveHost e.host; }) g.egress.allowLan
  ) cfg.guests);
  # Per-guest accepts first (first match wins); the RFC1918 drop is the floor beneath them.
  forwardRules =
    map (e: ''iifname "${bridge.name}" ip saddr ${e.ip} ip daddr ${e.target} tcp dport { ${lib.concatMapStringsSep ", " toString e.ports} } accept comment "${e.name} → ${e.host}"'') egressEntries
    ++ [ ''iifname "${bridge.name}" ip daddr { 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16 } drop comment "Guests: internet-only, never the LAN"'' ];
in
{
  imports = [ inputs.microvm.nixosModules.host ];

  options.homelab.microvm.host = {
    enable = lib.mkEnableOption "hosting microVM guests on an internal NAT bridge";
    uplink = lib.mkOption {
      type = lib.types.str;
      description = "Host interface guests egress through (NAT external interface).";
    };
    adminUser = lib.mkOption {
      type = lib.types.str;
      default = "bphenriques";
      description = "User allowed to `ssh -J` through to guests (TCP-forwarding override).";
    };
    bridge = {
      name = lib.mkOption { type = lib.types.str; };
      gateway = lib.mkOption { type = lib.types.str; };
      prefixLength = lib.mkOption { type = lib.types.int; };
    };
    guests = lib.mkOption {
      default = { };
      description = "Allocation table (imported from the host's guests.nix).";
      type = lib.types.attrsOf (lib.types.submodule {
        options = {
          ip = lib.mkOption { type = lib.types.str; };
          mac = lib.mkOption { type = lib.types.str; };
          vsockCid = lib.mkOption { type = lib.types.int; };
          autostart = lib.mkOption { type = lib.types.bool; default = true; };
          serviceConfig = lib.mkOption {
            type = lib.types.attrsOf lib.types.anything;
            default = { };
            description = ''
              Raw systemd serviceConfig (CPUQuota/MemoryMax/…), merged over the sandbox. Keys override
              per-key; list keys like DeviceAllow REPLACE — re-list /dev/kvm + /dev/net/tun to add a device.
            '';
          };
          monitoring = {
            traefikMetrics = lib.mkOption { type = lib.types.bool; default = false; };
            storageMount = lib.mkOption { type = lib.types.nullOr lib.types.str; default = null; };
          };
          egress.allowLan = lib.mkOption {
            default = [ ];
            description = ''
              Per-guest LAN allowlist (default: none). Each entry opens specific TCP ports on one LAN
              host; there is no all-ports form, so ssh stays closed unless a port is listed explicitly.
            '';
            type = lib.types.listOf (lib.types.submodule {
              options = {
                host = lib.mkOption { type = lib.types.str; description = "Fleet hostname (resolved via fleet.lan.hosts) or a raw IP."; };
                ports = lib.mkOption { type = lib.types.nonEmptyListOf lib.types.port; description = "TCP ports to open on that host."; };
              };
            });
          };
        };
      });
    };
  };

  # Top-level keys stay static (per-guest fan-out is inside the mapAttrs values); otherwise the
  # module's attr-paths would depend on cfg.guests and recurse via freeformType.
  config = lib.mkIf cfg.enable {
    # A guest may not allowlist the host's own LAN IP — that would route around the input-chain seal.
    assertions = map (e: {
      assertion = e.target != ownIp;
      message = "microvm guest ${e.name}: egress.allowLan may not target the host's own LAN IP (${e.host}) — it would bypass the host seal";
    }) egressEntries;

    # Narrow override of the no-forwarding baseline: reach guests via `ssh -J` (admin only).
    services.openssh.extraConfig = ''
      Match User ${cfg.adminUser}
        AllowTcpForwarding yes
    '';

    networking.nat = {
      enable = true;
      internalInterfaces = [ bridge.name ];
      externalInterface = cfg.uplink;
    };

    # Egress seal. Not firewall.extraForwardRules — those no-op unless filterForward=true and then
    # silently leak the LAN. Pre-firewall table (priority filter-1) drops guest→LAN/host terminally.
    networking.nftables.tables.microvm-containment = {
      family = "ip";
      content = ''
        chain forward {
          type filter hook forward priority filter - 1; policy accept;
          ${lib.concatStringsSep "\n          " forwardRules}
        }
        chain input {
          type filter hook input priority filter - 1; policy accept;
          iifname "${bridge.name}" ct state new drop comment "Guests never initiate to the host (return traffic is ct established)"
        }
      '';
    };

    # Internal bridge + tap enslave. ConfigureWithoutCarrier assigns the gateway IP with no VM up;
    # RequiredForOnline=no stops a VM-less boot hanging network-online.
    systemd.network = {
      netdevs."20-${bridge.name}".netdevConfig = { Kind = "bridge"; Name = bridge.name; };
      networks."20-${bridge.name}" = {
        matchConfig.Name = bridge.name;
        networkConfig = { Address = "${bridge.gateway}/${toString bridge.prefixLength}"; ConfigureWithoutCarrier = true; };
        linkConfig.RequiredForOnline = "no";
      };
      networks."30-vm-tap" = {
        matchConfig.Name = "vm-*";
        networkConfig.Bridge = bridge.name;
        linkConfig.RequiredForOnline = "no";
      };
    };

    # Sandbox as overridable per-key defaults, host-derived state path, then the guest's raw caps.
    systemd.services = lib.mapAttrs' (name: g:
      lib.nameValuePair "microvm@${name}" {
        serviceConfig = lib.mkMerge [
          (lib.mapAttrs (_: lib.mkDefault) vmSandbox)
          { ReadWritePaths = [ "${config.microvm.stateDir}/${name}" ]; }
          g.serviceConfig
        ];
      }
    ) cfg.guests;

    # Per-VM autostart; the microvm module derives the top-level microvm.autostart list from these.
    microvm.vms = lib.mapAttrs (_: g: { flake = self; restartIfChanged = true; inherit (g) autostart; }) cfg.guests;

    selfhost.monitoring.scopes = lib.mapAttrs mkScope cfg.guests;
  };
}
