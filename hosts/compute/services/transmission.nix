{ config, pkgs, ... }:
let
  pathsCfg = config.custom.paths;
  homelabMounts = config.custom.fileSystems.homelab.mounts;

  flood-for-transmission-wrapped = pkgs.runCommand "flood-for-transmission-wrapped" {} ''
    mkdir -p $out
    cp -r ${pkgs.flood-for-transmission}/* $out/
    chmod +w $out
    cp ${pkgs.flood-for-transmission}/config.json.defaults $out/config.json
  '';
in
{
  custom.home-server.routes.transmission.port = 9091;
  services.transmission = {
    enable = true;
    package = pkgs.transmission_4;
    settings = {
      download-dir = pathsCfg.media.downloads.root;
      incomplete_dir_enabled = true;
      incomplete-dir = pathsCfg.media.downloads.incomplete;
      rpc-port = config.custom.home-server.routes.transmission.port;
      rpc-host-whitelist-enabled = true;
      rpc-host-whitelist = config.custom.home-server.routes.transmission.host;
      idle_seeding_limit_enabled = true;
      idle_seeding_limit = 1; # 1 minute
    };
    webHome = pkgs.flood-for-transmission;
  };

  # Ensure transmission has the correct permissions to the directory and that it waits for the mounts
  users.users.${config.services.transmission.user}.extraGroups = [ homelabMounts.media.group ];
  systemd.services.transmission = {
    requires = [ homelabMounts.media.automountUnit ];
    after = [ homelabMounts.media.automountUnit ];
  };
}