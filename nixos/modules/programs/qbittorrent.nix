{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.services.qbittorrent;
  configDir = "${cfg.dataDir}/.config";
  openFilesLimit = 4096;
in
{
  options.services.qbittorrent = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Run qBittorrent headlessly as systemwide daemon
      '';
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.qbittorrent ];

    users.users = mkIf (cfg.user == "qbittorrent") {
      qbittorrent = {
        group = cfg.group;
        home = cfg.dataDir;
        createHome = true;
        description = "qBittorrent Daemon user";
      };
    };

    users.groups =
      mkIf (cfg.group == "qbittorrent") { qbittorrent = { gid = null; }; };
  };
}
