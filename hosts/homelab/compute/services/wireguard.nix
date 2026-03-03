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

  clients = lib.concatLists (
    lib.mapAttrsToList
      (_: u: map (d: { name = "${u.username}-${d}"; email = u.email; }) u.services.wireguard.devices)
      cfg.enabledUsers.wireguard
  );

  clientsJson = pkgs.writeText "wireguard-clients.json" (builtins.toJSON clients);

  wgEnv = {
    WG_DATA_DIR = dataDir;
    WG_INTERFACE = interface;
    WG_SERVER_ENDPOINT = "${endpoint}:${toString port}";
    WG_CLIENT_SUBNET = clientSubnet;
    WG_CLIENT_DNS = dns;
    WG_SERVER_ALLOWED_IPS = clientSubnet;
  } // lib.optionalAttrs (smtp.from != "") {
    WG_SMTP_FROM = smtp.from;
    WG_SMTP_URL_FILE = config.sops.templates."wireguard-smtp-url".path;
  };

  wgManagePkg = self.pkgs.wg-manage.override {
    homepageUrl = cfg.services.homepage.publicUrl;
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

  sops = {
    secrets."smtp-password" = { };
    templates."wireguard-smtp-url".content = "smtp://${smtp.user}:${config.sops.placeholder."smtp-password"}@${smtp.host}:${toString smtp.port}";
  };

  environment.systemPackages = [ wgManage ];
}
