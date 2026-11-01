# Host-agnostic microVM host: bridge, NAT, egress seal and per-guest taps, all driven by my.microvm.host.guests.
{ config, lib, self, inputs, ... }:
let
  cfg = config.my.microvm.host;
  inherit (cfg) bridge;

  inherit (import ./lib/sandboxes.nix) vmSandbox virtiofsdSandbox;

  ownIp = config.fleet.lan.hosts.${config.networking.hostName} or null;
  resolveHost = h: config.fleet.lan.hosts.${h} or h;
  hostNameOf = lib.mapAttrs' (name: ip: lib.nameValuePair ip name) config.fleet.lan.hosts;
  egressEntries = lib.concatLists (lib.mapAttrsToList (name: g:
    map (e: rec { inherit name; inherit (g) ip; inherit (e) host ports; target = resolveHost e.host; label = hostNameOf.${target} or e.host; }) g.egress.allowLan
  ) cfg.guests);
  guestSshTargets = lib.concatMapStringsSep " " (g: "${g.ip}:22") (lib.attrValues cfg.guests);

  hostPortGuests = lib.filterAttrs (_: g: g.egress.allowHostPorts != [ ]) cfg.guests;

  # daddr pin: without it the port also opens on the host's LAN address, which the forward drop never sees.
  inputRules =
    lib.mapAttrsToList (name: g:
      ''iifname "${bridge.name}" ip saddr ${g.ip} ip daddr ${bridge.gateway} tcp dport { ${lib.concatMapStringsSep ", " toString g.egress.allowHostPorts} } accept comment "${name} to host"''
    ) hostPortGuests
    ++ [ ''iifname "${bridge.name}" ct state new drop comment "Guests never initiate to the host (return traffic is ct established)"'' ];

  # Order matters: replies, then per-guest accepts, then the RFC1918 drop.
  forwardRules =
    [ ''iifname "${bridge.name}" ct state established,related accept comment "Replies to connections guests admitted"'' ]
    ++ map (e: ''iifname "${bridge.name}" ip saddr ${e.ip} ip daddr ${e.target} tcp dport { ${lib.concatMapStringsSep ", " toString e.ports} } accept comment "${e.name} to ${e.label}"'') egressEntries
    ++ [ ''iifname "${bridge.name}" ip daddr { 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16 } drop comment "Guests: internet-only, never the LAN"'' ];
in
{
  imports = [ inputs.microvm.nixosModules.host ];

  options.my.microvm.host = {
    enable = lib.mkEnableOption "hosting microVM guests on an internal NAT bridge";
    uplink = lib.mkOption {
      type = lib.types.str;
      description = "Host interface guests egress through.";
    };
    adminUser = lib.mkOption {
      type = lib.types.str;
      description = "User allowed to `ssh -J` through to guests.";
    };
    bridge = {
      name = lib.mkOption { type = lib.types.str; };
      gateway = lib.mkOption { type = lib.types.str; };
      prefixLength = lib.mkOption { type = lib.types.int; };
    };
    guests = lib.mkOption {
      default = { };
      description = "Guest allocation table.";
      type = lib.types.attrsOf (lib.types.submodule {
        options = {
          ip = lib.mkOption { type = lib.types.str; };
          mac = lib.mkOption { type = lib.types.str; };
          vsockCid = lib.mkOption { type = lib.types.int; };
          serviceConfig = lib.mkOption {
            type = lib.types.attrsOf lib.types.anything;
            default = { };
            description = "Raw serviceConfig merged over the sandbox; lists replace.";
          };
          egress.allowHostPorts = lib.mkOption {
            type = lib.types.listOf lib.types.port;
            default = [ ];
            description = "TCP ports on the host's bridge address this guest may reach.";
            example = [ 2222 ];
          };

          egress.allowLan = lib.mkOption {
            default = [ ];
            description = "LAN hosts and TCP ports this guest may reach.";
            type = lib.types.listOf (lib.types.submodule {
              options = {
                host = lib.mkOption { type = lib.types.str; description = "Fleet hostname or IP."; };
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
      }) egressEntries
      ++ lib.mapAttrsToList (name: g: {
        assertion = !(lib.elem 22 g.egress.allowHostPorts);
        message = "microvm guest ${name}: egress.allowHostPorts may not include 22; the host's sshd is reached the other way, through the adminUser PermitOpen override";
      }) hostPortGuests;

      # Exception to base.nix's AllowTcpForwarding = false.
      services.openssh.extraConfig = ''
        Match User ${cfg.adminUser}
          AllowTcpForwarding local
          PermitOpen ${guestSshTargets}
      '';

      # nixos-fw also sees these packets (priority 0), so open the ports there too, bridge only.
      networking.firewall.interfaces.${bridge.name}.allowedTCPPorts =
        lib.unique (lib.concatMap (g: g.egress.allowHostPorts) (lib.attrValues hostPortGuests));

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
            ${lib.concatStringsSep "\n  " forwardRules}
          }
          chain input {
            type filter hook input priority filter - 1; policy accept;
            ${lib.concatStringsSep "\n  " inputRules}
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
