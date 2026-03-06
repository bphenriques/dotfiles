{ config, lib, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.kavita;
  oidcCfg = config.custom.homelab.oidc;
  pathsCfg = config.custom.homelab.paths;
  homelabMounts = config.custom.homelab.cifs.mounts;

  kavitaCfg = config.services.kavita;
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.kavita = {
    port = 8097;
    secrets = {
      files = {
        token-key = { rotatable = true; bytes = 64; };
        admin-password = { rotatable = false; }; # TODO: admin-password could be removed if Kavita supports OIDC-only admin
      };
      systemd.dependentServices = [ "kavita" "kavita-configure" ];
    };
    oidc = {
      enable = true;
      callbackURLs = [
        "${serviceCfg.publicUrl}/signin-oidc"
        "${serviceCfg.publicUrl}/signout-callback-oidc"
      ];
      systemd.dependentServices = [ "kavita" ];
    };
    integrations.homepage = {
      enable = true;
      category = "Media";
      description = "Book Server";
    };
  };

  custom.homelab.cifs.mounts.media.systemd.dependentServices = [ "kavita" ];

  services.kavita = {
    enable = true;
    tokenKeyFile = serviceCfg.secrets.files.token-key.path;
    settings.Port = serviceCfg.port;
    settings.OpenIdConnectSettings = {
      Authority = oidcCfg.provider.url;
      ClientId = "@OIDC_CLIENT_ID@";
      Secret = "@OIDC_CLIENT_SECRET@";
    };
  };

  systemd.services.kavita = {
    serviceConfig = {
      LoadCredential = serviceCfg.oidc.systemd.loadCredentials;
      BindPaths = [
        "${pathsCfg.media.books.library}:/mnt/kavita/books"
        "${pathsCfg.media.comics.library}:/mnt/kavita/comics"
        "${pathsCfg.media.manga.library}:/mnt/kavita/manga"
      ];
    };
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
}
