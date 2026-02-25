{ config, lib, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.kavita;
  oidcCfg = config.custom.homelab.oidc;
  oidcClient = oidcCfg.clients.kavita;
  pathsCfg = config.custom.paths;
  homelabMounts = config.custom.fileSystems.homelab.mounts;

  kavitaCfg = config.services.kavita;
  credentialsDir = "${kavitaCfg.dataDir}/credentials";
  tokenKeyFile = "${credentialsDir}/token-key";
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.kavita = {
    port = 8097;
    dashboard = {
      enable = true;
      category = "Media";
      description = "Book Server";
      icon = "kavita.svg";
    };
  };

  custom.homelab.oidc.clients.kavita = {
    callbackURLs = [
      "${serviceCfg.publicUrl}/signin-oidc"
      "${serviceCfg.publicUrl}/signout-callback-oidc"
    ];
    systemd.dependentServices = [ "kavita" "kavita-configure" ];
  };

  services.kavita = {
    enable = true;
    tokenKeyFile = tokenKeyFile;
    settings.Port = serviceCfg.port;
  };

  systemd.tmpfiles.rules = [
    "d ${credentialsDir} 0700 ${kavitaCfg.user} ${kavitaCfg.user} -"
  ];

  users.users.kavita.extraGroups = [ homelabMounts.media.group ];
  custom.fileSystems.homelab.mounts.media.systemd.dependentServices = [ "kavita" ];

  systemd.services.kavita = {
    path = [ pkgs.openssl ];
    preStart = ''
      if [ ! -f "${tokenKeyFile}" ]; then
        openssl rand -base64 64 | tr -d '\n' > "${tokenKeyFile}"
        chmod 600 "${tokenKeyFile}"
      fi
    '';
    serviceConfig = {
      LoadCredential = oidcClient.systemd.loadCredentials;
      SupplementaryGroups = oidcClient.systemd.supplementaryGroups;
      BindPaths = [
        "${pathsCfg.media.books.library}:/mnt/media/books"
        "${pathsCfg.media.comics}:/mnt/media/comics"
      ];
    };
  };

  assertions = [
    {
      assertion = homelabMounts ? media;
      message = "Kavita requires custom.fileSystems.homelab.mounts.media to be configured.";
    }
  ];
}
