# Host-agnostic microVM host: bridge, NAT, egress seal and per-guest taps, all driven by my.microvm.host.guests.
{ config, lib, self, inputs, ... }:
let
  cfg = config.my.microvm.host;
  inherit (cfg) bridge;

  inherit (import ./lib/sandboxes.nix) vmSandbox virtiofsdSandbox;

  # LAN egress allowlist (default none), flattened once for both the nft accepts and the assertions.
  ownIp = config.fleet.lan.hosts.${config.networking.hostName} or null;
  resolveHost = h: config.fleet.lan.hosts.${h} or h;
  egressEntries = lib.concatLists (lib.mapAttrsToList (name: g:
    map (e: { inherit name; inherit (g) ip; inherit (e) host ports; target = resolveHost e.host; }) g.egress.allowLan
  ) cfg.guests);
  # Exact direct-tcpip targets for the sshd PermitOpen override.
  guestSshTargets = lib.concatMapStringsSep " " (g: "${g.ip}:22") (lib.attrValues cfg.guests);

  # Per-guest accepts first (first match wins); the RFC1918 drop is the floor beneath them.
  forwardRules =
    map (e: ''iifname "${bridge.name}" ip saddr ${e.ip} ip daddr ${e.target} tcp dport { ${lib.concatMapStringsSep ", " toString e.ports} } accept comment "${e.name} to ${e.host}"'') egressEntries
    ++ [ ''iifname "${bridge.name}" ip daddr { 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16 } drop comment "Guests: internet-only, never the LAN"'' ];
in
{
  imports = [ inputs.microvm.nixosModules.host ];

  options.my.microvm.host = {
    enable = lib.mkEnableOption "hosting microVM guests on an internal NAT bridge";
    uplink = lib.mkOption {
      type = lib.types.str;
      description = "Host interface guests egress through (NAT external interface).";
    };
    adminUser = lib.mkOption {
      type = lib.types.str;
      description = "User allowed to `ssh -J` through to guests (scoped local-forward override to guest :22 only).";
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
          serviceConfig = lib.mkOption {
            type = lib.types.attrsOf lib.types.anything;
            default = { };
            description = "Raw serviceConfig merged over the sandbox (a list key replaces, it does not append).";
          };
          egress.allowLan = lib.mkOption {
            default = [ ];
            description = "LAN hosts this guest may reach, per TCP port (default: none; there is no all-ports form).";
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

  config = lib.mkMerge [
    { microvm.host.enable = cfg.enable; }   # upstream defaults to true, and every host imports this module
    (lib.mkIf cfg.enable {
      assertions = [{
        assertion = cfg.guests != { };
        message = "my.microvm.host: enabled with an empty guest table; disable it instead";
      }] ++ map (e: {
        assertion = e.target != ownIp;
        message = "microvm guest ${e.name}: egress.allowLan may not target the host's own LAN IP (${e.host}), which would bypass the host seal";
      }) egressEntries;

      # Narrow override of the no-forwarding baseline: local forwards to a guest's :22, nothing else.
      services.openssh.extraConfig = ''
        Match User ${cfg.adminUser}
          AllowTcpForwarding local
          PermitOpen ${guestSshTargets}
      '';

      networking.nat = {
        enable = true;
        internalInterfaces = [ bridge.name ];
        externalInterface = cfg.uplink;
      };

      # Not firewall.extraForwardRules: those no-op unless filterForward=true, then leak the LAN.
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

      systemd.network = {
        netdevs."20-${bridge.name}".netdevConfig = { Kind = "bridge"; Name = bridge.name; };
        networks."20-${bridge.name}" = {
          matchConfig.Name = bridge.name;
          networkConfig = { Address = "${bridge.gateway}/${toString bridge.prefixLength}"; ConfigureWithoutCarrier = true; };   # assign the gateway IP with no VM up
          linkConfig.RequiredForOnline = "no";   # a VM-less boot must not hang network-online
        };
        networks."30-vm-tap" = {
          matchConfig.Name = "vm-*";
          networkConfig.Bridge = bridge.name;
          bridgeConfig.Isolated = true;   # block guest-to-guest at L2; the nft forward seal only sees routed traffic
          linkConfig.RequiredForOnline = "no";
        };
      };

      # Keys stay static: per-guest fan-out lives in the values, or attr-paths recurse via freeformType.
      systemd.services = lib.mapAttrs' (name: g:
        lib.nameValuePair "microvm@${name}" {
          serviceConfig = lib.mkMerge [
            (lib.mapAttrs (_: lib.mkDefault) vmSandbox)
            { ReadWritePaths = [ "${config.microvm.stateDir}/${name}" ]; }
            g.serviceConfig
          ];
        }
      ) cfg.guests
      // {
        "microvm-virtiofsd@".serviceConfig = lib.mapAttrs (_: lib.mkDefault) virtiofsdSandbox;
      };

      microvm.vms = lib.mapAttrs (_: _: { flake = self; restartIfChanged = true; }) cfg.guests;   # upstream defaults it false for flake-defined VMs
    })
  ];
}
