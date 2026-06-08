{ config, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.transmission;
  pathsCfg = config.custom.homelab.paths;
  homelabMounts = config.custom.homelab.smb.mounts;
  notifyCfg = config.custom.homelab.notify;
  serviceNotify = serviceCfg.integrations.notify;
  # Transmission runs these as subprocesses where LoadCredential isn't visible; the token file is
  # bind-mounted in (BindReadOnlyPaths below) so send-notification can read NOTIFY_TOKEN_FILE.
  torrentNotify = { title, tags }: pkgs.writeShellScript "torrent-notify" ''
    NOTIFY_URL=${notifyCfg.url} NOTIFY_TOKEN_FILE=${serviceNotify.tokenFile} \
      ${notifyCfg.package}/bin/send-notification \
      --topic ${serviceNotify.topic} --title "${title}" --tags "${tags}" --message "$TR_TORRENT_NAME"
  '';
in
{
  custom.homelab.services.transmission = {
    displayName = "Transmission";
    description = "Torrent Client";
    port = 9091;
    healthcheck.path = "/transmission/web/";
    access.allowedGroups = [ config.custom.homelab.groups.admin ];
    forwardAuth.enable = true;
    integrations.homepage.enable = true;
    integrations.notify.enable = true;
    integrations.notify.topic = "download";
    storage.smb = [ "media" ];
  };

  services.transmission = {
    enable = true;
    package = pkgs.transmission_4;
    settings = {
      download-dir = pathsCfg.media.downloads.root;
      incomplete_dir_enabled = true;
      incomplete-dir = pathsCfg.media.downloads.incomplete;
      rpc-port = serviceCfg.port;
      rpc-bind-address = "127.0.0.1";
      rpc-host-whitelist-enabled = true;
      rpc-host-whitelist = serviceCfg.publicHost;
      ratio-limit-enabled = true;
      ratio-limit = 1;
      idle_seeding_limit_enabled = true;
      idle_seeding_limit = 1;
      umask = 2;
      script-torrent-added-enabled = true;
      script-torrent-added-filename = toString (torrentNotify { title = "Download Started"; tags = "arrow_down"; });
      script-torrent-done-enabled = true;
      script-torrent-done-filename = toString (torrentNotify { title = "Download Complete"; tags = "white_check_mark"; });
    };
  };

  users.users.${config.services.transmission.user}.extraGroups = [ homelabMounts.media.group ];

  systemd.services.transmission.serviceConfig = {
    Restart = "on-failure";
    RestartSec = "10s";
    RestartMaxDelaySec = "5min";
    RestartSteps = 5;
    BindReadOnlyPaths = [ serviceCfg.integrations.notify.tokenFile ];
  };
}
