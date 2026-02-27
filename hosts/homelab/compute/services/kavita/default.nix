{ config, lib, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.kavita;
  oidcCfg = config.custom.homelab.oidc;
  pathsCfg = config.custom.paths;
  homelabMounts = config.custom.fileSystems.homelab.mounts;

  kavitaCfg = config.services.kavita;
  credentialsDir = "${kavitaCfg.dataDir}/credentials";
  tokenKeyFile = "${credentialsDir}/token-key";
in
{
  imports = [ ./configure.nix ];

  # It is possible to login using admin but requires query parameter
  custom.homelab = {
    services.kavita = {
      port = 8097;
      dashboard = {
        enable = true;
        category = "Media";
        description = "Book Server";
        icon = "kavita.svg";
      };
      # TODO: https://gethomepage.dev/widgets/services/kavita/
    };

    oidc.clients.kavita = {
      callbackURLs = [
        "${serviceCfg.publicUrl}/signin-oidc"
        "${serviceCfg.publicUrl}/signout-callback-oidc"
      ];
      # kavita reads OIDC credentials from appsettings.json (injected in preStart)
      systemd.dependentServices = [ "kavita" ];
    };
  };

  # Generate token-key before kavita.service starts (LoadCredential requires the file to exist)
  systemd.tmpfiles.rules = [ "d ${credentialsDir} 0700 ${kavitaCfg.user} ${kavitaCfg.user} -" ];
  systemd.services.kavita-init = {
    description = "Initialize Kavita token key";
    unitConfig.ConditionPathExists = "!${tokenKeyFile}";
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      User = kavitaCfg.user;
      Group = kavitaCfg.user;
      UMask = "0077";
    };
    path = [ pkgs.openssl ];
    script = ''openssl rand -base64 64 | tr -d '\n' > "${tokenKeyFile}"'';
  };

  services.kavita = {
    enable = true;
    tokenKeyFile = tokenKeyFile;
    settings.Port = serviceCfg.port;
    # OIDC Authentication - credentials injected via preStart from OIDC provisioning
    settings.OpenIdConnectSettings = {
      Authority = oidcCfg.provider.url;
      ClientId = "@OIDC_CLIENT_ID@";
      Secret = "@OIDC_CLIENT_SECRET@";
    };
  };
  systemd.services.kavita = {
    after = [ "kavita-init.service" ];
    requires = [ "kavita-init.service" ];
    serviceConfig = {
      LoadCredential = oidcCfg.clients.kavita.systemd.loadCredentials;
      BindPaths = [
        "${pathsCfg.media.books.library}:/mnt/kavita/books"
        "${pathsCfg.media.comics.library}:/mnt/kavita/comics"
        "${pathsCfg.media.manga.library}:/mnt/kavita/manga"
      ];
    };
    # Inject OIDC credentials into appsettings.json after NixOS module writes it
    preStart = lib.mkAfter ''
      ${pkgs.replace-secret}/bin/replace-secret '@OIDC_CLIENT_ID@' \
        "''${CREDENTIALS_DIRECTORY}/oidc-id" \
        '${kavitaCfg.dataDir}/config/appsettings.json'
      ${pkgs.replace-secret}/bin/replace-secret '@OIDC_CLIENT_SECRET@' \
        "''${CREDENTIALS_DIRECTORY}/oidc-secret" \
        '${kavitaCfg.dataDir}/config/appsettings.json'
    '';
  };
  users.users.kavita.extraGroups = [ homelabMounts.media.group ];
  custom.fileSystems.homelab.mounts.media.systemd.dependentServices = [ "kavita" ];
}
