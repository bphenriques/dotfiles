{ config, lib, pkgs, ... }:
let
  cfg = config.custom.home-server.wireguard;
  dataDir = "/var/lib/wireguard";
  serverKeyFile = "${dataDir}/server/private.key";
  serverPubKeyFile = "${dataDir}/server/public.key";

  clientsJson = pkgs.writeText "wireguard-clients.json" (builtins.toJSON cfg.clients);

  # Derive subnet from address (e.g., 10.100.0.1/24 -> 10.100.0.0/24)
  addressParts = lib.splitString "/" cfg.address;
  ipParts = lib.splitString "." (builtins.head addressParts);
  cidr = builtins.elemAt addressParts 1;
  defaultSubnet = "${builtins.elemAt ipParts 0}.${builtins.elemAt ipParts 1}.${builtins.elemAt ipParts 2}.0/${cidr}";

  clientSubnet = if cfg.clientSubnet != null then cfg.clientSubnet else defaultSubnet;

  wgEnv = {
    WG_DATA_DIR = dataDir;
    WG_INTERFACE = cfg.interface;
    WG_SERVER_ENDPOINT = "${cfg.endpoint}:${toString cfg.port}";
    WG_CLIENT_SUBNET = clientSubnet;
    WG_CLIENT_DNS = cfg.dns;
  } // lib.optionalAttrs (cfg.allowedIPs != null) {
    WG_SERVER_ALLOWED_IPS = cfg.allowedIPs;
  } // lib.optionalAttrs (cfg.smtpFrom != "") {
    WG_SMTP_FROM = cfg.smtpFrom;
  };

  script = pkgs.writeTextFile {
    name = "wg-manage.nu";
    text = lib.fileContents ../.././../packages/wg-manage/script.nu;
  };

  wgManage = pkgs.writeShellApplication {
    name = "wg-manage";
    runtimeInputs = with pkgs; [ nushell wireguard-tools qrencode coreutils mutt ];
    text = ''
      ${lib.concatStringsSep "\n" (lib.mapAttrsToList (k: v: "export ${k}=\"${v}\"") wgEnv)}
      exec nu ${script} "$@"
    '';
    meta.platforms = lib.platforms.linux;
  };
in
{
  options.custom.home-server.wireguard = {
    enable = lib.mkEnableOption "WireGuard VPN server";

    interface = lib.mkOption {
      type = lib.types.str;
      default = "wg0";
    };

    port = lib.mkOption {
      type = lib.types.int;
      default = 51820;
    };

    address = lib.mkOption {
      type = lib.types.str;
      description = "Server address with CIDR (e.g., 10.100.0.1/24)";
    };

    endpoint = lib.mkOption {
      type = lib.types.str;
      description = "Public endpoint for clients";
    };

    clientSubnet = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Client address pool (defaults to network derived from address)";
    };

    allowedIPs = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Allowed IPs for client tunnels (defaults to clientSubnet, use 0.0.0.0/0 for full tunnel)";
    };

    dns = lib.mkOption {
      type = lib.types.str;
      description = "DNS server for clients";
    };

    clients = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Declarative client names (IPs auto-assigned)";
    };

    smtpFrom = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "From address for email (requires mutt configured)";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.tmpfiles.rules = [
      "d ${dataDir} 0700 root root -"
      "d ${dataDir}/server 0700 root root -"
      "d ${dataDir}/clients 0700 root root -"
    ];

    systemd.services.wireguard-keygen = {
      description = "Generate WireGuard server keys";
      wantedBy = [ "wireguard-${cfg.interface}.service" ];
      before = [ "wireguard-${cfg.interface}.service" ];
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
      description = "Bootstrap WireGuard peers";
      wantedBy = [ "multi-user.target" ];
      after = [ "wireguard-${cfg.interface}.service" ];
      requires = [ "wireguard-${cfg.interface}.service" ];
      restartTriggers = [ clientsJson ];
      path = [ wgManage ];
      serviceConfig = { Type = "oneshot"; RemainAfterExit = true; };
      script = ''
        wg-manage bootstrap ${lib.optionalString (cfg.clients != [ ]) clientsJson}
      '';
    };

    networking.wireguard.interfaces.${cfg.interface} = {
      ips = [ cfg.address ];
      listenPort = cfg.port;
      privateKeyFile = serverKeyFile;
    };

    networking.firewall.allowedUDPPorts = [ cfg.port ];
    boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

    environment.systemPackages = [ wgManage ];
  };
}
