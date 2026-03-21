{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.actual-budget;
  oidcCfg = config.custom.homelab.oidc;
in
{
  imports = [ ./backup.nix ];

  custom.homelab.services.actual-budget = {
    displayName = "Actual Budget";
    metadata.description = "Budget Manager";
    metadata.version = config.services.actual.package.version;
    metadata.homepage = config.services.actual.package.meta.homepage;
    metadata.category = "General";
    port = 5006;
    subdomain = "budget";
    access.allowedGroups = [ config.custom.homelab.groups.admin ];
    oidc = {
      enable = true;
      callbackURLs = [ "${serviceCfg.publicUrl}/openid/callback" ];
      systemd.dependentServices = [ "actual" ];
    };
    integrations.homepage.enable = true;
  };

  services.actual = {
    enable = true;
    settings = {
      hostname = serviceCfg.host;
      port = serviceCfg.port;
      loginMethod = "openid";
      userCreationMode = "login";
      trustedProxies = [ "127.0.0.1" "::1" ];
      openId = {
        discoveryURL = oidcCfg.provider.issuerUrl;
        client_id._secret = serviceCfg.oidc.id.file;
        client_secret._secret = serviceCfg.oidc.secret.file;
        server_hostname = serviceCfg.publicUrl;
      };
    };
  };

  systemd.services.actual.serviceConfig.SupplementaryGroups = serviceCfg.oidc.systemd.supplementaryGroups;
}
