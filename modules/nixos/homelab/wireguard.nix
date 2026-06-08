# WireGuard VPN: server interface, key generation, per-user device registry, and client
# provisioning. The server generates each client keypair; `wg-manage show <name>` renders a QR to
# scan onto the device in person (no email). LAN routing (forward + NAT) is an opt-in nftables impl
# (lanAccess); disable it and build your own from the exposed `peers` list. Gated, opt-in like smb.
{ config, lib, pkgs, self, ... }:
let
  cfg = config.custom.homelab;
  wg = cfg.wireguard;

  fullAccessPeers = builtins.filter (c: c.fullAccess) wg.peers;

  # Deny-by-default: only established, allowed subnets, and fullAccess clients forward; rest dropped.
  forwardRules =
    [ "ct state established,related accept" ]
    ++ lib.concatMap (s: [ "ip saddr ${s} accept" "ip daddr ${s} accept" ]) wg.lanAccess.extraAllowedSubnets
    ++ map (c: ''iifname "${wg.interface}" ip saddr ${c.ip} accept'') fullAccessPeers
    ++ [ ''iifname "${wg.interface}" drop'' ];

  nftablesContent = ''
    chain forward {
      type filter hook forward priority 0; policy drop;
      ${lib.concatStringsSep "\n      " forwardRules}
    }
  '' + lib.optionalString wg.lanAccess.masquerade ''
    chain postrouting {
      type nat hook postrouting priority srcnat; policy accept;
      ip saddr ${wg.clientSubnet} ip daddr ${wg.lanAccess.subnet} masquerade
    }
  '';

  enabledUsers = lib.filterAttrs (_: u: u.services.wireguard.enable) cfg.users;

  dataDir = "/var/lib/wireguard";
  serverKeyFile = "${dataDir}/server/private.key";
  serverPubKeyFile = "${dataDir}/server/public.key";

  clients = lib.concatLists (
    lib.mapAttrsToList
      (_: u: map (d: {
        name = "${u.username}-${d.name}";
        device = d.name;
        inherit (d) ip fullAccess;
      }) u.services.wireguard.devices)
      enabledUsers
  );

  clientsJson = pkgs.writeText "wireguard-clients.json" (builtins.toJSON clients);

  wgEnv = {
    WG_DATA_DIR = dataDir;
    WG_INTERFACE = wg.interface;
    WG_HOMELAB_NAME = wg.name;
    WG_SERVER_ENDPOINT = "${wg.endpoint}:${toString wg.listenPort}";
    WG_CLIENT_SUBNET = wg.clientSubnet;
    WG_CLIENT_DNS = wg.dns;
    WG_SERVER_ALLOWED_IPS = lib.concatStringsSep ","
      ([ wg.clientSubnet ] ++ lib.optional wg.lanAccess.enable wg.lanAccess.subnet);
  };

  wgManage = pkgs.writeShellApplication {
    name = "wg-manage";
    runtimeInputs = [ self.packages.wg-manage ];
    text = ''
      ${lib.concatStringsSep "\n" (lib.mapAttrsToList (k: v: "export ${k}=\"${v}\"") wgEnv)}
      exec wg-manage-bin "$@"
    '';
  };
in
{
  options.custom.homelab.wireguard = {
    enable = lib.mkEnableOption "WireGuard VPN server (interface, keys, user/device registry, client provisioning)";

    interface = lib.mkOption {
      type = lib.types.str;
      default = "wg0";
      description = "WireGuard interface name.";
    };
    listenPort = lib.mkOption {
      type = lib.types.port;
      default = 51820;
      description = "WireGuard UDP listen port (opened in the firewall).";
    };
    address = lib.mkOption {
      type = lib.types.str;
      description = "Server address with CIDR (e.g. 10.100.0.1/24).";
    };
    clientSubnet = lib.mkOption {
      type = lib.types.str;
      description = "Client address subnet (e.g. 10.100.0.0/24).";
    };
    endpoint = lib.mkOption {
      type = lib.types.str;
      description = "Public endpoint host/IP that clients dial.";
    };
    dns = lib.mkOption {
      type = lib.types.str;
      description = "DNS server pushed to clients.";
    };
    name = lib.mkOption {
      type = lib.types.str;
      description = "Short identity prefix for client interface/device names (e.g. 'bphenr').";
    };
    openFirewall = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Open the WireGuard listen UDP port in the firewall.";
    };

    lanAccess = {
      enable = lib.mkEnableOption "opt-in nftables forwarding/NAT so clients reach the LAN (else clients reach only the server)";
      subnet = lib.mkOption {
        type = lib.types.str;
        description = "LAN subnet full-access clients may reach; added to their AllowedIPs and used as the masquerade destination. Required when lanAccess.enable.";
      };
      extraAllowedSubnets = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        example = [ "10.88.0.0/15" ];
        description = "Extra subnets allowed to forward through the wg host (e.g. container networks).";
      };
      masquerade = lib.mkEnableOption "srcnat masquerade of client traffic into the LAN (enable only if the LAN lacks routes back to the client subnet)";
    };

    peers = lib.mkOption {
      type = lib.types.listOf (lib.types.attrsOf lib.types.anything);
      readOnly = true;
      default = clients;
      defaultText = lib.literalMD "derived from `users.*.services.wireguard`";
      description = ''
        Derived per-device peers (from users.*.services.wireguard), for consumer firewall/routing
        rules. Each entry: { name, device, ip, fullAccess }.
      '';
    };
  };

  options.custom.homelab.users = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      options.services.wireguard = {
        enable = lib.mkEnableOption "WireGuard configuration for this user";
        devices = lib.mkOption {
          type = lib.types.listOf (lib.types.submodule {
            options = {
              name = lib.mkOption {
                type = lib.types.strMatching "[a-z0-9][a-z0-9-]*";
                description = "Device name (e.g. phone, laptop). Lowercase alphanumeric and dashes only.";
              };
              ip = lib.mkOption {
                type = lib.types.str;
                description = "Static WireGuard client IP (e.g. 10.100.0.42).";
              };
              fullAccess = lib.mkOption {
                type = lib.types.bool;
                default = false;
                description = "If true, device can reach the whole LAN; if false, only the home server.";
              };
            };
          });
          default = [ ];
          description = "WireGuard devices for this user.";
        };
      };
    });
  };

  config = lib.mkIf wg.enable (lib.mkMerge [ {
    custom.homelab.services.wireguard = {
      description = "VPN";
      port = wg.listenPort;
      ingress.enable = false;
      integrations.monitoring = {
        healthcheck = false;
        exporters.wireguard = {
          enable = true;
          listenAddress = "127.0.0.1";
          port = 9586;
          latestHandshakeDelay = true;
        };
        scrapeConfigs = [{
          job_name = "wireguard";
          static_configs = [{
            targets = [ "127.0.0.1:9586" ];
            labels.instance = config.networking.hostName;
          }];
        }];
      };
    };

    systemd.tmpfiles.rules = [
      "d ${dataDir} 0700 root root -"
      "d ${dataDir}/server 0700 root root -"
      "d ${dataDir}/clients 0700 root root -"
    ];

    systemd.services.wireguard-keygen = {
      description = "WireGuard keygen";
      wantedBy = [ "wireguard-${wg.interface}.service" ];
      before = [ "wireguard-${wg.interface}.service" ];
      after = [ "systemd-tmpfiles-setup.service" ];
      serviceConfig.Type = "oneshot";
      path = [ pkgs.wireguard-tools ];
      script = ''
        if [ ! -f "${serverKeyFile}" ]; then
          echo "Generating Wireguard key..."
          wg genkey > ${serverKeyFile}
          chmod 0600 ${serverKeyFile}
          wg pubkey < ${serverKeyFile} > ${serverPubKeyFile}
        else
          echo "Wireguard key already exists."
        fi
        echo "Wireguard key ready."
      '';
    };

    systemd.services.wireguard-bootstrap = {
      description = "WireGuard bootstrap";
      wantedBy = [ "multi-user.target" ];
      after = [ "wireguard-${wg.interface}.service" ];
      requires = [ "wireguard-${wg.interface}.service" ];
      restartTriggers = [ clientsJson ];
      path = [ wgManage ];
      serviceConfig = { Type = "oneshot"; RemainAfterExit = true; };
      script = ''wg-manage bootstrap ${clientsJson}'';
    };

    networking.wireguard.interfaces.${wg.interface} = {
      ips = [ wg.address ];
      inherit (wg) listenPort;
      privateKeyFile = serverKeyFile;
    };

    networking.firewall.allowedUDPPorts = lib.optionals wg.openFirewall [ wg.listenPort ];

    assertions = let
      clientsByIp = builtins.groupBy (c: c.ip) clients;
      ipCollisions = lib.filterAttrs (_: cs: builtins.length cs > 1) clientsByIp;

      maxIfNameLen = 15;
      prefix = "${wg.name}-";
      maxDeviceLen = maxIfNameLen - builtins.stringLength prefix;
      tooLong = builtins.filter (c: builtins.stringLength c.device > maxDeviceLen) clients;
    in [{
      assertion = ipCollisions == { };
      message = "WireGuard IP collision detected: ${lib.concatStringsSep ", " (
        lib.mapAttrsToList (ip: cs: "${ip} -> [${lib.concatMapStringsSep ", " (c: c.name) cs}]") ipCollisions
      )}";
    } {
      assertion = tooLong == [ ];
      message = "WireGuard device name too long (max ${toString maxDeviceLen} chars with prefix '${prefix}'): ${
        lib.concatMapStringsSep ", " (c: "'${c.device}' (${toString (builtins.stringLength c.device)} chars)") tooLong
      }";
    }];

    environment.systemPackages = [ wgManage ];
  }

    (lib.mkIf wg.lanAccess.enable {
      boot.kernel.sysctl."net.ipv4.ip_forward" = 1;
      networking.nftables.enable = true;
      networking.nftables.tables.wireguard-access = {
        family = "inet";
        content = nftablesContent;
      };
    })
  ]);
}
