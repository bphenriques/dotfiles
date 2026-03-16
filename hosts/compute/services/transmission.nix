{ config, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.transmission;
  pathsCfg = config.custom.homelab.paths;
  homelabMounts = config.custom.homelab.smb.mounts;
  ntfyCfg = config.custom.homelab.services.ntfy;
  ntfyNotify = { title, tags }: pkgs.writeShellScript "torrent-notify" ''
    ${pkgs.curl}/bin/curl -s \
      -H "Authorization: Bearer $(cat ${serviceCfg.integrations.ntfy.tokenFile})" \
      -H "Title: ${title}" \
      -H "Tags: ${tags}" \
      -d "$TR_TORRENT_NAME" \
      "${ntfyCfg.url}/${serviceCfg.integrations.ntfy.topic}"
  '';
in
{
  custom.homelab.services.transmission = {
    description = "Torrent Client";
    version = config.services.transmission.package.version;
    homepage = config.services.transmission.package.meta.homepage;
    category = "Media";
    port = 9091;
    healthcheck.path = "/transmission/web/";
    forwardAuth.enable = true;
    integrations.homepage.enable = true;
    integrations.ntfy.enable = true;
    integrations.ntfy.topic = "download";
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
      script-torrent-added-filename = toString (ntfyNotify { title = "Download Started"; tags = "arrow_down"; });
      script-torrent-done-enabled = true;
      script-torrent-done-filename = toString (ntfyNotify { title = "Download Complete"; tags = "white_check_mark"; });
    };
  };

  users.users.${config.services.transmission.user}.extraGroups = [ homelabMounts.media.group ];
  custom.homelab.smb.mounts.media.systemd.dependentServices = [ "transmission" ];

  systemd.services.transmission.serviceConfig = {
    Restart = "on-failure";
    RestartSec = "10s";
    RestartMaxDelaySec = "5min";
    RestartSteps = 5;
    BindReadOnlyPaths = [ serviceCfg.integrations.ntfy.tokenFile ];
  };
}
