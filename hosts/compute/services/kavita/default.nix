{ config, lib, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.kavita;
  oidcCfg = config.custom.homelab.oidc;
  pathsCfg = config.custom.homelab.paths;
  homelabMounts = config.custom.homelab.smb.mounts;

  kavitaCfg = config.services.kavita;
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.kavita = {
    displayName = "Kavita";
    metadata.description = "Book Server";
    metadata.version = config.services.kavita.package.version;
    metadata.homepage = config.services.kavita.package.meta.homepage;
    metadata.category = "Media";
    port = 8097;
    secrets = {
      files = {
        token-key = { rotatable = true; bytes = 64; };
        admin-password = { rotatable = false; }; # TODO: admin-password could be removed if Kavita supports OIDC-only admin
      };
      systemd.dependentServices = [ "kavita" "kavita-configure" ];
    };
    access.allowedGroups = with config.custom.homelab.groups; [ guests users admin ];
    oidc = {
      enable = true;
      callbackURLs = [
        "${serviceCfg.publicUrl}/signin-oidc"
        "${serviceCfg.publicUrl}/signout-callback-oidc"
      ];
      systemd.dependentServices = [ "kavita" ];
    };
    healthcheck.path = "/api/health";
    integrations.homepage.enable = true;
  };

  custom.homelab.smb.mounts.media.systemd.dependentServices = [ "kavita" ];

  services.kavita = {
    enable = true;
    tokenKeyFile = serviceCfg.secrets.files.token-key.path;
    settings.Port = serviceCfg.port;
    settings.OpenIdConnectSettings = {
      Authority = oidcCfg.provider.issuerUrl;
      ClientId = serviceCfg.oidc.id.placeholder;
      Secret = serviceCfg.oidc.secret.placeholder;
    };
  };

  systemd.services.kavita = {
    serviceConfig = {
      LoadCredential = serviceCfg.oidc.systemd.loadCredentials;
      BindReadOnlyPaths = [
        "${pathsCfg.media.books.library}:/mnt/kavita/books"
        "${pathsCfg.media.comics.library}:/mnt/kavita/comics"
        "${pathsCfg.media.manga.library}:/mnt/kavita/manga"
      ];
    };
    # Kavita has no native _FILE support for OIDC credentials; replace placeholders at runtime.
    # If upstream adds file-based config, switch to it and remove this workaround.
    preStart = lib.mkAfter ''
      ${pkgs.replace-secret}/bin/replace-secret '${serviceCfg.oidc.id.placeholder}' \
        "''${CREDENTIALS_DIRECTORY}/oidc-id" \
        '${kavitaCfg.dataDir}/config/appsettings.json'
      ${pkgs.replace-secret}/bin/replace-secret '${serviceCfg.oidc.secret.placeholder}' \
        "''${CREDENTIALS_DIRECTORY}/oidc-secret" \
        '${kavitaCfg.dataDir}/config/appsettings.json'
    '';
  };

  users.users.kavita.extraGroups = [ homelabMounts.media.group ];
}
