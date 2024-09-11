{ config, pkgs, ... }:
{
  home.packages = with pkgs; [ discord ];
  systemd.user.tmpfiles.rules = [
    "d ${config.xdg.configHome}/discord/Cache/Cache_Data - - - 10d" # Clean cache older than 10 days
  ];
}
