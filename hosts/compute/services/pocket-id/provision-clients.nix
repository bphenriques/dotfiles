# Per-client OIDC provisioning after Pocket-ID starts.
# Each client gets its own systemd unit for independent rotation.
# Credentials are written to /run/homelab-oidc/<client>/{id,secret}.
{ config, pkgs, lib, self, ... }:
let
  cfg = config.custom.homelab.oidc;
  pocketIdCfg = config.custom.homelab.services.pocket-id;
  baseServiceName = "pocket-id-provision-base";

  mkClientConfigFile = name: client:
    pkgs.writeText "oidc-client-config-${name}.json" (builtins.toJSON {
      inherit (client) name callbackURLs pkce;
    });
in
{
  custom.homelab.oidc.systemd.clientProvisionUnitPrefix = "pocket-id-provision-client-";

  systemd.services = lib.mapAttrs' (name: client: let
    clientConfigFile = mkClientConfigFile name client;
  in lib.nameValuePair "pocket-id-provision-client-${name}" {
    description = "Pocket-ID OIDC client provisioning for ${name}";
    wantedBy = [ "pocket-id.service" ];
    after = [ "network-online.target" "pocket-id.service" "${baseServiceName}.service" ];
    wants = [ "network-online.target" ];
    requires = [ "pocket-id.service" "${baseServiceName}.service" ];
    partOf = [ "pocket-id.service" ];
    restartTriggers = [ clientConfigFile ./oidc-provision-client.nu ];
    startLimitIntervalSec = 300;
    startLimitBurst = 3;
    environment = {
      POCKET_ID_URL = pocketIdCfg.url;
      POCKET_ID_API_KEY_FILE = cfg.provider.apiKeyFile;
      OIDC_CLIENT_CONFIG_FILE = toString clientConfigFile;
      OIDC_CREDENTIALS_DIR = cfg.credentials.dir;
    };
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      Restart = "on-failure";
      RestartSec = 10;
      PrivateTmp = true;
      ProtectSystem = "strict";
      ReadWritePaths = [ cfg.credentials.dir ];
      ProtectHome = true;
      NoNewPrivileges = true;
      RestrictAddressFamilies = [ "AF_INET" "AF_INET6" "AF_UNIX" ];
    };
    path = [ pkgs.nushell ];
    script = ''nu ${self.lib.builders.writeNushellScript "oidc-provision-client" ./oidc-provision-client.nu}'';
  }) cfg.clients;
}
