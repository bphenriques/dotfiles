# Security Model:
# - Access control is enforced SERVER-SIDE via nftables, not client-side AllowedIPs
# - All clients receive full AllowedIPs in their config (routing convenience only)
# - Nix config is the single source of truth for access permissions:
#   - fullAccess=true:  client can forward traffic to the entire network
#   - fullAccess=false: client can only reach this server (forwarding blocked)
# - Clients created via CLI (wg-manage add) are always restricted
# - Client IPs are deterministic (hash of name) for stable firewall rules
#
{ config, lib, pkgs, self, ... }:
let
  cfg = config.custom.homelab;
  
  interface = "wg0";
  port = 51820;
  address = "10.100.0.1/24";
  clientSubnet = "10.100.0.0/24";
  dns = "1.1.1.1";
  endpoint = self.settings.services.wireguard.endpoint;
  smtp = self.settings.smtp;

  dataDir = "/var/lib/wireguard";
  serverKeyFile = "${dataDir}/server/private.key";
  serverPubKeyFile = "${dataDir}/server/public.key";

  serverIp = lib.head (lib.splitString "/" address);

  # Deterministic IP from client name hash: .1 = server, .2-.9 = CLI ad-hoc (via next_ip), .10-.254 = Nix-managed
  clientIp = name: let
    nixClientIpStart = 10;
    nixClientIpEnd = 254;
    hash = builtins.hashString "sha256" name;
    nixClientIpRange = nixClientIpEnd - nixClientIpStart + 1;  # 245 addresses
    octet = lib.mod (lib.fromHexString (builtins.substring 0 8 hash)) nixClientIpRange + nixClientIpStart;
    subnetPrefix = lib.concatStringsSep "." (lib.take 3 (lib.splitString "." serverIp));
  in "${subnetPrefix}.${toString octet}";

  clients = lib.concatLists (
    lib.mapAttrsToList
      (_: u: map (d: let name = "${u.username}-${d.name}"; in {
        inherit name;
        ip = clientIp name;
        email = u.email;
        fullAccess = d.fullAccess;
      }) u.services.wireguard.devices)
      cfg.enabledUsers.wireguard
  );

  fullAccessClients = builtins.filter (c: c.fullAccess) clients;

  clientsJson = pkgs.writeText "wireguard-clients.json" (builtins.toJSON clients);

  wgEnv = {
    WG_DATA_DIR = dataDir;
    WG_INTERFACE = interface;
    WG_SERVER_ENDPOINT = "${endpoint}:${toString port}";
    WG_SERVER_IP = serverIp;
    WG_CLIENT_SUBNET = clientSubnet;
    WG_CLIENT_DNS = dns;
    WG_SERVER_ALLOWED_IPS = clientSubnet;
  } // lib.optionalAttrs (smtp.from != "") {
    WG_SMTP_FROM = smtp.from;
    WG_SMTP_URL_FILE = config.sops.templates."wireguard-smtp-url".path;
  };

  wgManagePkg = self.pkgs.wg-manage.override {
    homepageUrl = cfg.services.homepage.publicUrl;
    emailSubject = "🏠 Home Sweet (Remote) Home: WireGuard VPN key inside";
    emailTemplateMd = ./email-template.md;
  };

  wgManage = pkgs.writeShellApplication {
    name = "wg-manage";
    runtimeInputs = [ wgManagePkg ];
    text = ''
      ${lib.concatStringsSep "\n" (lib.mapAttrsToList (k: v: "export ${k}=\"${v}\"") wgEnv)}
      exec wg-manage-bin "$@"
    '';
  };
in
{
  systemd.tmpfiles.rules = [
    "d ${dataDir} 0700 root root -"
    "d ${dataDir}/server 0700 root root -"
    "d ${dataDir}/clients 0700 root root -"
  ];

  systemd.services.wireguard-keygen = {
    description = "Generate WireGuard server keys";
    wantedBy = [ "wireguard-${interface}.service" ];
    before = [ "wireguard-${interface}.service" ];
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
    after = [ "wireguard-${interface}.service" ];
    requires = [ "wireguard-${interface}.service" ];
    restartTriggers = [ clientsJson ];
    path = [ wgManage ];
    serviceConfig = { Type = "oneshot"; RemainAfterExit = true; };
    script = ''wg-manage bootstrap ${clientsJson}'';
  };

  networking.wireguard.interfaces.${interface} = {
    ips = [ address ];
    listenPort = port;
    privateKeyFile = serverKeyFile;
  };

  networking.firewall.allowedUDPPorts = [ port ];
  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

  # Firewall (forward chain only, not input):
  # - Default: drop all forwarding (policy drop)
  # - Allow LAN access only to fullAccess clients
  # - Restricted clients can still reach server services, just not forward to LAN
  # - No input chain restrictions needed: access control is managed through OIDC+ForwardAuth
  networking.nftables.enable = true;
  networking.nftables.tables.wireguard-access = {
    family = "inet";
    content = ''
      chain forward {
        type filter hook forward priority 0; policy accept;

        # Allow established/related connections (return traffic)
        ct state established,related accept

        # Allow fullAccess WireGuard clients to forward to network
        ${lib.concatMapStringsSep "\n        " (c: "iifname \"${interface}\" ip saddr ${c.ip} accept") fullAccessClients}

        # Drop all other WireGuard client forwarding (restricted clients can only reach server)
        iifname "${interface}" drop
      }
    '';
  };

  sops = {
    secrets."smtp-password" = { };
    templates."wireguard-smtp-url".content = "smtp://${smtp.user}:${config.sops.placeholder."smtp-password"}@${smtp.host}:${toString smtp.port}";
  };

  assertions = let
    clientsByIp = lib.groupBy (c: c.ip) clients;
    ipCollisions = lib.filterAttrs (_: cs: builtins.length cs > 1) clientsByIp;
  in [{
    assertion = ipCollisions == { };
    message = "WireGuard IP collision detected: ${lib.concatStringsSep ", " (
      lib.mapAttrsToList (ip: cs: "${ip} -> [${lib.concatMapStringsSep ", " (c: c.name) cs}]") ipCollisions
    )}";
  }];

  environment.systemPackages = [ wgManage ];
}
